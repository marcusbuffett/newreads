ENV_FILE=.prod.env 
export $(xargs < $ENV_FILE) 
./render_templates.py 
echo "BLAH"
echo $TRAEFIK_PORT
SERVICES="web server jaeger db ackee ackee-mongo traefik"
docker-compose stop $SERVICES
docker-compose up --build -d $SERVICES
