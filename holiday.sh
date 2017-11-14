apt-get update
apt-get install -y wget curl git tmux postgresql build-essential libssl-dev make automake autoconf libncurses5-dev gcc default-jre

curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
mv kerl /usr/local/bin/
kerl build 20.1 20.1
mkdir -p /srv/otp/20.1/
kerl install 20.1 /srv/otp/20.1/
source /srv/otp/20.1/activate

certbot certonly --standalone -d holidayping.lambdaclass.com

sudo -u postgres createuser
sudo -u postgres createdb

/etc/letsencrypt/live/holidayping.lambdaclass.com/
privkey.pem
fullchain.pem
