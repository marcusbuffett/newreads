# Newreads

Newreads is a site to exchange book recommendations. It lives at [https://nextgreatbook.com](https://nextgreatbook.com) currently.

Newreads is open source, donates all revenue (from affiliate links) to
[GiveDirectly](https://www.givedirectly.org/), ~~and it's transparent~~ (update:
I like the open metrics idea and I'll try to make it happen, but the setup got
screwed up in the migration to kubernetes, not sure how to make it open now,
ideas welcome).

## Running locally

### Db, traefik, ackee, etc.

To avoid repeating my configs between dev and prod I've got a python script that
will fill jinja templates in [docker_templates](docker_templates) with env
variables. So you'll have to export the right env file, run the python script,
then run docker-compose, something like this:

```
pip install jinja2
ENV_FILE=.dev.env && export $(xargs < $ENV_FILE) && python render_templates.py && docker-compose up db traefik ackee
```

This will start the PostgreSQL db, and traefik for routing. Ackee is optional,
it's just for analytics.

### Server

```
cd server
stack run -- --recreatetables --scrapemockbooks --mockdata --startserver
```

Somewhat self-explanatory, but this will create the db tables, scrape some books
from goodreads (defined in [MockBooks.hs](./server/app/MockBooks.hs)),
seed some data, then start the server.

For future invocations you'll only need to run the server:

```
cd server
stack run -- --startserver
```

### Client

```
cd frontend
yarn start
```

Keep in mind this will open 127.0.0.1:4201, but you'll need to go to
http://web.lvh.me:4200 instead, since that's where traefik is running, which proxies
api requests to the server.

## Tech stack

- Backend
  - [Haskell](https://www.haskell.org/)
  - [Selda](https://selda.link/)
  - [Scotty](https://hackage.haskell.org/package/scotty)
  - [Scalpel](https://hackage.haskell.org/package/scalpel)
- Frontend
  - [Purescript](https://www.purescript.org/)
  - [Halogen](https://github.com/purescript-halogen/purescript-halogen)
- Etc.
  - [Docker compose](https://docs.docker.com/compose/)
  - [kubernetes](https://kubernetes.io/)
  - [Kong Ingress Controller](https://docs.konghq.com/2.1.x/kong-for-kubernetes/)
  - [Traefik](https://containo.us/traefik/)
  - [Ackee](https://ackee.electerious.com/)

### Frontend - Purescript

The frontend is written in PureScript, using the Halogen framework. It's a bit
of a mess for all the usual reasons a project in a never-before-used language
is, but feel free to check it out [here](./frontend/src) (or for a
digestible component, check out [the recommendation
card](./frontend/src/Component/RecommendationCard.purs)). Shout out to
the PureScript community for being fucking awesome.

### Backend - Haskell

The backend is written in Haskell. For DB stuff I'm using Selda (all the models
live [in Models.hs](./server/app/Models.hs)). For [the goodreads
scraping](./server/app/BookUrlsScraping.hs) I use Scalpel, and for [the
server](./server/app/Server.hs) I use Scotty. The text search is powered
by a fork I've made of full-text-search, to get around some performance issues
with indexing 100s of thousands of documents.
