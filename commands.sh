# Connecting from server container
pgcli -U nwreads_server --port $DB_PORT --host $DB_HOST -d nwreads
# Start scraping container
ENV_FILE=.prod.env && \
  export $(xargs < $ENV_FILE) && \
  ./render_templates.py && \
  docker-compose build scraper && \
  docker-compose run scraper bash
# Scraping
./server --scraperecursive
# Docker compose logs
ENV_FILE=.prod.env && export $(xargs < $ENV_FILE)  && docker-compose logs traefik
ENV_FILE=.prod.env && export $(xargs < $ENV_FILE) && ./render_templates.py && docker-compose build scraper && docker-compose run scraper bash

# Install commands

helm install kong/kong --generate-name --set ingressController.installCRDs=false
linkerd install | kubectl apply -f -
