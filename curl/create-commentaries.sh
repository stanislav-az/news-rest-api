echo 'Running comments create scripts'

echo
echo '1-1'
curl -X POST --data '{"content": "My favourite number is 1", "user_id": 1}' localhost:8080/api/posts/1/comments
echo
echo '1-2'
curl -X POST --data '{"content": "My favourite number is 2", "user_id": 2}' localhost:8080/api/posts/1/comments
echo
echo '1-3'
curl -X POST --data '{"content": "My favourite number is 3", "user_id": 3}' localhost:8080/api/posts/1/comments
echo
echo '1-4'
curl -X POST --data '{"content": "My favourite number is 4", "user_id": 4}' localhost:8080/api/posts/1/comments
echo
echo '1-5'
curl -X POST --data '{"content": "My favourite number is 5", "user_id": 5}' localhost:8080/api/posts/1/comments
echo
echo 'done!'