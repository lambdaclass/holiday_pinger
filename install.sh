apt-get update
apt-get install -y wget curl git tmux postgresql build-essential libssl-dev make automake autoconf libncurses5-dev gcc default-jre certbot

# install erlang
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
mv kerl /usr/local/bin/
kerl build 20.1 20.1
mkdir -p /srv/otp/20.1/
kerl install 20.1 /srv/otp/20.1/
source /srv/otp/20.1/activate

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

# get ssl certificates
certbot certonly --standalone -d holidayping.lambdaclass.com

# TODO add sed to modiy pems and user, passwords and other things in config
#/etc/letsencrypt/live/holidayping.lambdaclass.com/privkey.pem
#/etc/letsencrypt/live/holidayping.lambdaclass.com/fullchain.pem

#generate release
make release

# run release
_build/prod/rel/holiday_ping/bin/holiday_ping start
_build/prod/rel/holiday_ping/bin/holiday_ping attach
