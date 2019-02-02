echo 'Running user create scripts'

echo '1-1'
curl -X POST --data '{"name": "User1", "surname": "Userovich1", "avatar": "http1"}' localhost:8080/api/users
echo '1-2'
curl -X POST --data '{"name": "User2", "surname": "Userovich2", "avatar": "http2"}' localhost:8080/api/users
echo '1-3'
curl -X POST --data '{"name": "User3", "surname": "Userovich3", "avatar": "http3"}' localhost:8080/api/users
echo '1-4'
curl -X POST --data '{"name": "User4", "surname": "Userovich4", "avatar": "http4"}' localhost:8080/api/users
echo '1-5'
curl -X POST --data '{"name": "User5", "surname": "Userovich5", "avatar": "http5"}' localhost:8080/api/users
echo
echo 'done!'