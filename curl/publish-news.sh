echo 'Running news publish scripts'

echo
echo '1-1'
curl -X POST --header 'Authorization: 1' localhost:8080/api/posts/1
echo
echo '1-2'
curl -X POST --header 'Authorization: 2' localhost:8080/api/posts/2
echo
echo '1-3'
curl -X POST --header 'Authorization: 3' localhost:8080/api/posts/3
echo
echo '1-4'
curl -X POST --header 'Authorization: 4' localhost:8080/api/posts/4
echo
echo '1-5'
curl -X POST --header 'Authorization: 5' localhost:8080/api/posts/5
echo
echo 'done!'