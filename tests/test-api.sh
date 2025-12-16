#!/bin/bash

# Тестирование API проекта Кассиопея

set -e

echo "=== Тестирование API ==="
echo "Время: $(date)"

# Цвета для вывода
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Базовый URL
BASE_URL="http://localhost:8080"

# Проверка здоровья сервисов
echo -e "\n${YELLOW}1. Проверка здоровья сервисов:${NC}"

services=("/health" "/api/iss/last" "/api/jwst/feed" "/api/astro/events")

for service in "${services[@]}"; do
    response=$(curl -s -o /dev/null -w "%{http_code}" "${BASE_URL}${service}")
    
    if [ "$response" -eq 200 ]; then
        echo -e "${GREEN}✓ ${service} - HTTP ${response}${NC}"
    else
        echo -e "${RED}✗ ${service} - HTTP ${response}${NC}"
    fi
done

# Тестирование rate limit
echo -e "\n${YELLOW}2. Тестирование rate limiting:${NC}"

echo "Отправка 15 запросов за 1 секунду..."
for i in {1..15}; do
    status=$(curl -s -o /dev/null -w "%{http_code}" "${BASE_URL}/api/iss/last")
    
    if [ "$status" -eq 429 ]; then
        echo -e "${GREEN}✓ Rate limit сработал на запросе ${i}${NC}"
        break
    fi
    
    if [ "$i" -eq 15 ]; then
        echo -e "${RED}✗ Rate limit не сработал${NC}"
    fi
done

# Тестирование Redis
echo -e "\n${YELLOW}3. Проверка Redis кэширования:${NC}"

first_time=$(curl -s "${BASE_URL}/api/iss/last" | jq -r '.fetched_at' 2>/dev/null || echo "null")
sleep 1
second_time=$(curl -s "${BASE_URL}/api/iss/last" | jq -r '.fetched_at' 2>/dev/null || echo "null")

if [ "$first_time" == "$second_time" ] && [ "$first_time" != "null" ]; then
    echo -e "${GREEN}✓ Кэширование работает${NC}"
else
    echo -e "${RED}✗ Проблемы с кэшированием${NC}"
fi

# Тестирование CSV экспорта
echo -e "\n${YELLOW}4. Тестирование CSV экспорта:${NC}"

curl -s "${BASE_URL}/api/iss/export?format=csv" -o /tmp/test.csv

if [ -s /tmp/test.csv ]; then
    lines=$(wc -l < /tmp/test.csv)
    echo -e "${GREEN}✓ CSV экспорт работает (${lines} строк)${NC}"
else
    echo -e "${RED}✗ Ошибка CSV экспорта${NC}"
fi

# Проверка безопасности
echo -e "\n${YELLOW}5. Проверка безопасности:${NC}"

# Проверка CSRF
csrf_test=$(curl -s -X POST "${BASE_URL}/upload" -F "file=@/etc/passwd")
if echo "$csrf_test" | grep -q "CSRF"; then
    echo -e "${GREEN}✓ CSRF защита работает${NC}"
else
    echo -e "${RED}✗ Проблемы с CSRF защитой${NC}"
fi

# Проверка XSS
xss_test=$(curl -s "${BASE_URL}/page/unsafe")
if echo "$xss_test" | grep -q "<script>"; then
    echo -e "${RED}✗ XSS уязвимость обнаружена${NC}"
else
    echo -e "${GREEN}✓ XSS защита работает${NC}"
fi

echo -e "\n${YELLOW}=== Тестирование завершено ===${NC}"