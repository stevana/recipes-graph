# Recipes graph

[![Build
status](https://github.com/stevana/recipes-graph/actions/workflows/main.yaml/badge.svg)](https://github.com/stevana/recipes-graph/actions)

This repository contains a food recipe database and a program that generates
static [webpages](https://stevana.github.io/recipes-graph) from said database.

More specifically, it takes a yaml file containing [recipes](data/recipes.yaml)
as input, generates an SQLite [graph
database](https://github.com/dpapathanasiou/simple-graph), queries the database,
generates HTML pages for each query and uploads the HTML to GitHub pages.

People who know SQL and have SQLite installed can also download the
[database](https://stevana.github.io/recipes-graph/recipes.sqlite) and query it
directly.

## To do

- [ ] Query by time the dish takes to make
- [ ] Query by ingredient(s)
- [ ] Optionally use javascript to select multiple queries/filters
- [ ] Integrate [sql.js](https://github.com/sql-js/sql.js/tree/master/examples/GUI)?
