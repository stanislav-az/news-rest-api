echo 'Running category create scripts'

echo
echo '1-1'
curl -X POST --data '{"name": "cat1"}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-2'
curl -X POST --data '{"name": "cat2"}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-3'
curl -X POST --data '{"name": "cat3"}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-4'
curl -X POST --data '{"name": "cat4", "parent_id": 1}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-5'
curl -X POST --data '{"name": "cat5", "parent_id": 2}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-6'
curl -X POST --data '{"name": "cat6", "parent_id": 3}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-7'
curl -X POST --data '{"name": "cat7", "parent_id": 4}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-8'
curl -X POST --data '{"name": "cat8", "parent_id": 5}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo '1-9'
curl -X POST --data '{"name": "cat9", "parent_id": 6}' --header 'Authorization: 0' localhost:8080/api/categories
echo
echo 'done!'
