echo 'Running news create scripts'

echo
echo '1-1'
curl -X POST --data '{"title": "Breaking news 1","author_id": 1,"category_id": 1,"content": "Nothing happened 1 time","main_photo": "http1","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-2'
curl -X POST --data '{"title": "Breaking news 2","author_id": 2,"category_id": 2,"content": "Nothing happened 2 times","main_photo": "http2","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-3'
curl -X POST --data '{"title": "Breaking news 3","author_id": 3,"category_id": 3,"content": "Nothing happened 3 times","main_photo": "http3","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-4'
curl -X POST --data '{"title": "Breaking news 4","author_id": 4,"category_id": 4,"content": "Nothing happened 4 times","main_photo": "http4","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-5'
curl -X POST --data '{"title": "Breaking news 5","author_id": 5,"category_id": 5,"content": "Nothing happened 5 times","main_photo": "http5","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-6'
curl -X POST --data '{"title": "Breaking news 6","author_id": 1,"category_id": 6,"content": "Nothing happened 6 times","main_photo": "http6","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-7'
curl -X POST --data '{"title": "Breaking news 7","author_id": 2,"category_id": 7,"content": "Nothing happened 7 times","main_photo": "http7","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-8'
curl -X POST --data '{"title": "Breaking news 8","author_id": 3,"category_id": 8,"content": "Nothing happened 8 times","main_photo": "http8","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-9'
curl -X POST --data '{"title": "Breaking news 9","author_id": 4,"category_id": 9,"content": "Nothing happened 9 times","main_photo": "http9","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo '1-10'
curl -X POST --data '{"title": "Breaking news 10","author_id": 5,"category_id": 1,"content": "Nothing happened 10 times","main_photo": "http10","tags": [1,2,3], "photos": ["url1", "url2"]}' localhost:8080/api/posts
echo
echo 'done!'
