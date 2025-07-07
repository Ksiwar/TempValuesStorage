# Temporary Values Storage 

## API Endpoints

```
POST   /keys      - Создать запись
GET    /keys      - Перечислить все записи юзера
GET    /keys/:key - Получить запись по ключк
PUT    /keys/:key - Изменить запись
DELETE /keys/:key - Удалить запись
```

## Аутентификация
```
Authorization: Basic <base64(username:password)>
```

## Билд и запуск
```bash
rebar3 compile
rebar3 shell
```

## Тесты 
```bash
rebar3 eunit
```

## Пробные curl (на всякий)
```
curl -X POST -H "Authorization: Basic $(echo -n 'user1:pass' | base64)" -H "Content-Type: application/json" -d '{"key":"test_key", "value":{"foo":"bar"}}' http://localhost:8080/keys
```

```
curl -H "Authorization: Basic $(echo -n 'user1:pass' | base64)" \
     http://localhost:8080/keys/test_key
```