#!/usr/bin/env bash

apt-get update
apt-get install -y wget curl git tmux postgresql build-essential libssl-dev make automake autoconf libncurses5-dev gcc default-jre certbot nginx ufw python3-pip lzop pv

# install erlang
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
mv kerl /usr/local/bin/
kerl build 20.1 20.1
mkdir -p /srv/otp/20.1/
kerl install 20.1 /srv/otp/20.1/
source /srv/otp/20.1/activate
echo "source /srv/otp/20.1/activate" >> .bashrc

# add leningen
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod +x lein
mv lein /usr/local/bin/

# generate postgres user
sudo -u postgres createuser holiday_ping
sudo -u postgres createdb holiday_ping
sudo -u postgres psql -c "ALTER ROLE holiday_ping WITH SUPERUSER;"
sudo -u postgres psql -c "ALTER USER holiday_ping WITH PASSWORD 'holiday_ping';"

# run migrations
cp /root/holiday_ping/priv/sql/tables.sql /tmp/
sudo -u postgres psql -a -f /tmp/tables.sql

# setup db backups
echo '
wal_level = replica
' >> /etc/postgresql/9.6/main/postgresql.conf
service postgresql restart

sudo python3 -m pip install wal-e[aws]
echo "cd /var/lib/postgresql/
sudo -u postgres \
  AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY AWS_REGION=us-east-1 wal-e \
  -k $AWS_ACCESS_KEY_ID \
  --s3-prefix=s3://lambdaclass.com/holiday_ping/backups \
  backup-push /var/lib/postgresql/9.6/main

sudo -u postgres \
  AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY AWS_REGION=us-east-1 wal-e \
  -k $AWS_ACCESS_KEY_ID \
  --s3-prefix=s3://lambdaclass.com/holiday_ping/backups \
  delete --confirm retain 5
" > /usr/bin/holiday-backup
chmod +x /usr/bin/holiday-backup
echo '0 0 * * * /usr/bin/holiday-backup > /var/log/holiday-backup.log 2>&1' | crontab

# get ssl certificates. issue with already running port
certbot certonly --standalone -d holidayping.lambdaclass.com

#add nginx
echo 'server {
       listen         80;
       server_name    holidayping.lambdaclass.com;
       return         301 https://$server_name$request_uri;
}' > /etc/nginx/sites-available/default

service nginx start
systemctl enable nginx

#generate release
make release

# run release
/root/holiday_ping/_build/prod/rel/holiday_ping/bin/holiday_ping start
/root/holiday_ping/_build/prod/rel/holiday_ping/bin/holiday_ping attach

# DON'T ASK. Scaleway issue with ufw. Check https://community.online.net/t/how-to-configures-iptables-with-input-rules-with-dynamic-nbd/303/22
sed -i -e "s/DEFAULT_INPUT_POLICY=\"DROP\"/DEFAULT_INPUT_POLICY=\"ACCEPT\"/" /etc/default/ufw
awk '!found && /COMMIT/ { print "-A ufw-reject-input -j DROP"; found=1 } 1' /etc/ufw/after.rules > /tmp/after.rules && mv /tmp/after.rules /etc/ufw/after.rules

ufw allow ssh
ufw allow https
ufw allow http
ufw enable

systemctl enable ufw
