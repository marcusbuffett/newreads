# Newreads

Newreads is a site to exchange book recommendations. It's open source,
transparent, and donates all revenue (from affiliate links) to
[GiveDirectly](https://www.givedirectly.org/). It lives at [https://nextgreatbook.com](https://nextgreatbook.com) currently.

## Running

To avoid repeating my configs between dev and prod I've got a python script that
will fill jinja templates in [docker_templates](docker_templates) with env
variables. So you'll have to export the right env file, run the python script,
then run docker-compose, something like this:

```
ENV_FILE=.dev.env && export $(xargs < $ENV_FILE) && python render_templates.py && docker-compose up
```

## Analytics

You can access all the analytics for the site
[here](https://ackee.nextgreatbook.com), just login with admin/admin.

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
  - [Traefik](https://containo.us/traefik/)
  - [Ackee](https://ackee.electerious.com/)

### Frontend - Purescript

The frontend is written in PureScript, using the Halogen framework. It's a bit
of a mess for all the usual reasons a project in a never-before-used language
is, but feel free to check it out [here](./frontend-halogen/src) (or for a
digestible component, check out [the recommendation
card](./frontend-halogen/src/Component/RecommendationCard.purs)). Shout out to
the PureScript community for being fucking awesome.

### Backend - Haskell

The backend is written in Haskell. For DB stuff I'm using Selda (all the models
live [in Models.hs](./nwdir-server/app/Models.hs)). For [the goodreads
scraping](./nwdir-server/app/BookUrlsScraping.hs) I use Scalpel, and for [the
server](./nwdir-server/app/Server.hs) I use Scotty. The text search is powered
by a fork I've made of full-text-search, to get around some performance issues
with indexing 100s of thousands of documents.
