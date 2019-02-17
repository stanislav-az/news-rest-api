echo 'Running main scripts'

echo
echo '1***** PATCH /authors *****'
echo '1-1 responds with 404 without correct authorization'
curl -X PATCH --data '{"description": "NEW"}' --header 'Authorization: 66' localhost:8080/api/authors/1
echo
echo '1-2 responds with 200 with correct authorization'
curl -X PATCH --data '{"description": "NEW"}' --header 'Authorization: 0' localhost:8080/api/authors/1
echo
echo '2***** PATCH /tags *****'
echo '2-1 responds with 404 without correct authorization'
curl -X PATCH --data '{"name": "NEW"}' --header 'Authorization: 66' localhost:8080/api/tags/1
echo
echo '2-2 responds with 200 with correct authorization'
curl -X PATCH --data '{"name": "NEW"}' --header 'Authorization: 0' localhost:8080/api/tags/1
echo
echo '3***** PATCH /categories *****'
echo '3-1 responds with 404 without correct authorization'
curl -X PATCH --data '{"name": "NEW"}' --header 'Authorization: 66' localhost:8080/api/categories/1
echo
echo '3-2 responds with 200 with correct authorization patching name'
curl -X PATCH --data '{"name": "NEW"}' --header 'Authorization: 0' localhost:8080/api/categories/1
echo
echo '3-3 responds with 200 with correct authorization patching parent id'
curl -X PATCH --data '{"parent_id": 3}' --header 'Authorization: 0' localhost:8080/api/categories/1
echo
echo '3-4 responds with 200 with correct authorization patching both'
curl -X PATCH --data '{"name": "NEW", "parent_id": 3}' --header 'Authorization: 0' localhost:8080/api/categories/1
echo
echo '4***** PUBLISH /posts *****'
echo '4-1 responds with 404 without correct authorization'
curl -X POST --header 'Authorization: 66' localhost:8080/api/posts/6
echo
echo '4-2 responds with 200 with correct authorization '
curl -X POST --header 'Authorization: 1' localhost:8080/api/posts/6
echo
echo '4-3 responds with 200 with admin authorization '
curl -X POST --header 'Authorization: 0' localhost:8080/api/posts/7
echo
echo '5***** PATCH /posts *****'
echo '5-1 responds with 404 without correct authorization'
curl -X PATCH --data '{"title": "NEW", "category_id": 3, "content": "updated", "main_photo": "new"}' --header 'Authorization: 66' localhost:8080/api/posts/6
echo
echo '5-2 responds with 200 with correct authorization '
curl -X PATCH --data '{"title": "NEW", "category_id": 3, "content": "updated", "main_photo": "new"}' --header 'Authorization: 1' localhost:8080/api/posts/6
echo
echo '5-3 responds with 200 with admin authorization '
curl -X PATCH --data '{"title": "NEW", "category_id": 3, "content": "updated", "main_photo": "new"}' --header 'Authorization: 0' localhost:8080/api/posts/7
echo
echo '6***** DELETE /posts *****'
echo '6-1 responds with 404 without correct authorization'
curl -X DELETE --header 'Authorization: 66' localhost:8080/api/posts/8
echo
echo '6-2 responds with 204 with correct authorization '
curl -X DELETE --header 'Authorization: 3' localhost:8080/api/posts/8
echo
echo '6-3 responds with 204 with admin authorization '
curl -X DELETE --header 'Authorization: 0' localhost:8080/api/posts/9
echo
echo '7***** GET /posts *****'
echo '7-1 responds with 200'
curl -X GET 'localhost:8080/api/posts?limit=15&offset=0&author_name=Author2&category_id=2&title_has=breaking&content_has=hap&created_at_gt=2019-01-09&tag_id=1&sort_by=date'
echo
echo '8***** SEARCH /posts *****'
echo '8-1 responds with 200'
curl -X GET 'localhost:8080/api/posts/search/aut?limit=15&offset=0&sort_by=name'
echo
echo '9***** GET /comments *****'
echo '9-1 responds with 200'
curl -X GET 'localhost:8080/api/posts/1/comments?limit=15&offset=0'
echo
echo 'done!'
