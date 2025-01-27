# docker compose down --rmi "all" -v --remove-orphans
docker compose down -v --remove-orphans

docker compose up -d

docker compose logs app -f
