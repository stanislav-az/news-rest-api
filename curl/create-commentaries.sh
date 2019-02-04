echo 'Running comments create scripts'

echo
echo '1-1'
curl -X POST --data '{"content": "My favourite number is 1"}' localhost:8080/api/posts/1/comments
echo
echo '1-2'
curl -X POST --data '{"content": "My favourite number is 2"}' localhost:8080/api/posts/1/comments
echo
echo '1-3'
curl -X POST --data '{"content": "My favourite number is 3"}' localhost:8080/api/posts/1/comments
echo
echo '1-4'
curl -X POST --data '{"content": "My favourite number is 4"}' localhost:8080/api/posts/1/comments
echo
echo '1-5'
curl -X POST --data '{"content": "My favourite number is 5"}' localhost:8080/api/posts/1/comments
echo
echo 'done!'