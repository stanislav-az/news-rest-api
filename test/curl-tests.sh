echo 'Run curl tests script'

echo 'Running tag create scripts'

echo
echo '1-1'
curl -X POST --data '{"name": "programming"}' --header 'Authorization: 1' localhost:8080/api/tag
echo
echo '1-2'
curl -X POST --data '{"name": "sport"}' --header 'Authorization: 1' localhost:8080/api/tag
echo
echo '1-3'
curl -X POST --data '{"name": "music"}' --header 'Authorization: 1' localhost:8080/api/tag
echo
echo 'done!'