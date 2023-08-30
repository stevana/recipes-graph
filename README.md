# Recipes graph

[![Build
status](https://github.com/stevana/recipes-graph/actions/workflows/main.yaml/badge.svg)](https://github.com/stevana/recipes-graph/actions)

This repository contains a food recipe database and a program that generates
static [webpages](https://stevana.github.io/recipes-graph) from said database.

More specifically, it takes a yaml file containing [recipes](data/recipes.yaml)
as input, generates an SQLite [graph
database](https://github.com/dpapathanasiou/simple-graph), queries the database,
generates HTML pages for each query and uploads the HTML to GitHub pages.

## To do

- [ ] Query by time the dish takes to make
- [ ] Query by ingredient(s)
- [ ] Optionally use javascript to select multiple queries/filters
