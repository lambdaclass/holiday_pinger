apt-get update
apt-get install -y wget curl git tmux postgresql build-essential libssl-dev make automake autoconf libncurses5-dev gcc default-jre certbot nginx ufw

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
sudo -u postgres createuser holiay_ping
sudo -u postgres createdb holiday_ping
sudo -u postgres psql -c "ALTER ROLE holiday_ping WITH SUPERUSER;"
sudo -u postgres psql -c "ALTER USER holiday_ping WITH PASSWORD 'holiday_ping';"

# run migrations
cp /root/holiday_ping/priv/sql/tables.sql /tmp/
sudo -u postgres psql -a -f /tmp/tables.sql

# get ssl certificates. issue with already running port
certbot certonly --standalone -d holidayping.lambdaclass.com

#add nginx
echo 'server {
       listen         80;
       server_name    holidayping.lambdaclass.com;
       return         301 https://$server_name$request_uri;
}' > /etc/nginx/sites-available/default

#generate release
make release

# run release
/root/holiday_ping/_build/prod/rel/holiday_ping/bin/holiday_ping start
/root/holiday_ping/_build/prod/rel/holiday_ping/bin/holiday_ping attach

# add ufw
sed -i -e "s/DEFAULT_INPUT_POLICY=\"DROP\"/DEFAULT_INPUT_POLICY=\"ACCEPT\"/" /etc/default/ufw
awk '!found && /COMMIT/ { print "-A ufw-reject-input -j DROP"; found=1 } 1' /etc/ufw/after.rules > /tmp/after.rules && mv /tmp/after.rules /etc/ufw/after.rules

ufw allow ssh
ufw allow https
ufw allow http
ufw enable
