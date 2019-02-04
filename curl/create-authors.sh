echo 'Running author create scripts'

echo
echo '1-1'
curl -X POST --data '{"name": "Author1", "surname": "Authorovich1", "avatar": "http1", "description": "Philologist1"}' --header 'Authorization: 0' localhost:8080/api/authors
echo
echo '1-2'
curl -X POST --data '{"name": "Author2", "surname": "Authorovich2", "avatar": "http2", "description": "Philologist2"}' --header 'Authorization: 0' localhost:8080/api/authors
echo
echo '1-3'
curl -X POST --data '{"name": "Author3", "surname": "Authorovich3", "avatar": "http3", "description": "Philologist3"}' --header 'Authorization: 0' localhost:8080/api/authors
echo
echo '1-4'
curl -X POST --data '{"name": "Author4", "surname": "Authorovich4", "avatar": "http4", "description": "Philologist4"}' --header 'Authorization: 0' localhost:8080/api/authors
echo
echo '1-5'
curl -X POST --data '{"name": "Author5", "surname": "Authorovich5", "avatar": "http5", "description": "Philologist5"}' --header 'Authorization: 0' localhost:8080/api/authors
echo
echo 'done!'