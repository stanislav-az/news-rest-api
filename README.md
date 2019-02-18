# news-rest-api
This is a REST API news web server, runs on [warp](http://hackage.haskell.org/package/warp), uses [wai](http://hackage.haskell.org/package/wai-3.2.2) and PostgreSQL DBMS. It has no frontend, so it only responds in raw JSON data. Server starts at http://localhost:8080/. 
## :gear: Functionality (relative URLs)
### GET api/posts/
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.
* Supports filtering:
  - By date created. Get news posted at exact date with ?created_at, or posted earlier with ?created_at_lt, or posted later with ?created_at_gt.
  - By author name (case ignored) with ?author_name.
  - By category id with ?category_id.
  - By tag id. Get posts with exact tag with ?tag_id, including some tags in a list with ?tag_id_in, including all tags in a list with ?tag_id_all.
  - By phrase in title (case ignored) with ?title_has.
  - By phase in content (case ignored) with ?content_has.
* Supports sorting with ?sort_by:
  - By author name ?sort_by=author.
  - By category name ?sort_by=category.
  - By date created ?sort_by=date.
  - By number of photos ?sort_by=photos.
### GET api/posts/search/yoursearchqueryhere
* Finds search query in content or in a author/tag/category name.
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.
* Supports sorting with ?sort_by:
  - By author name ?sort_by=author.
  - By category name ?sort_by=category.
  - By date created ?sort_by=date.
  - By number of photos ?sort_by=photos.
### GET api/authors
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.
### GET api/users
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.
### GET api/tags
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.
### GET api/categories
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.
### GET api/posts/1/comments
* Gets comments to a specific post by its id.
* Supports pagination. To receive paginated data provide query parameters of limit and offset. Example: ?limit=15&offset=0.

## :ledger: Logging
Supported levels of logging are: Debug, Info, Warn, Error, the log writes to `<./logs/news-server.log>` by default. 
## :wrench: Configurability
Configuration file is `<./conf/configuration.local>`. Configure database connection, pagination and logging. For convenience there is `<./conf/configuration.template>` file in the repository, that could be used for reference. 
## :computer: App using guide
### Building
To build an executable use `<stack build>` command. 
### Database migration
Use `<stack repl>` command to load project in interactive mode, then call `initializeDB` function. SQL files in `<./migrations>` will be then executed in alphabetical order. Note that you have to configure connection at `<./conf/configuration.local>` and create database first (for example with psql).
### Running tests
To test project use `<stack test>` command. Curl tests are located at `<./curl/>`, use `<./curl/run-tests.sh>` to run them all. 
### Launching
To launch app use `<stack exec news-rest-api-exe>` command.
