SELECT COUNT(*)
FROM goodreadsBooks
where filled = true;


SELECT Title
FROM goodreadsBooks
where filled = true;

WITH cte AS
  (SELECT "GoodreadsId",
          ROW_NUMBER() OVER (PARTITION BY "GoodreadsId"
                             ORDER BY "GoodreadsId") row_num
   FROM "goodreadsBooks")
DELETE
FROM cte
WHERE row_num > 1;


delete
from "goodreadsBooks"
where "GoodreadsId" in
    (with cte as
       (select "GoodreadsId" as id,
               row_number() over(partition by "GoodreadsId"
                                 order by "GoodreadsId" desc) as rn
        from "goodreadsBooks") select id
     from cte
     where rn > 1);


delete
from "goodreadsBooks"
where "Uuid" in
    (with cte as
       (select "Uuid" as id,
               row_number() over(partition by "Uuid"
                                 order by "Uuid" desc) as rn
        from "goodreadsBooks") select id
     from cte
     where rn > 1);


SELECT Requests."Uuid",
       "goodreadsBooks"."Title"
FROM "recRequests" as Requests
INNER JOIN "goodreadsBooks" ON Requests."BookUuid"="goodreadsBooks"."Uuid"
INNER JOIN users ON Requests."ProfileUuid"="users"."Uuid"
ORDER BY Requests."Created" DESC;


ALTER TABLE "goodreadsBooks" ADD CONSTRAINT unique_goodreadsid UNIQUE ("GoodreadsId");


CREATE INDEX goodreadsBooks_ratings ON "goodreadsBooks" ("NumRatings");


ALTER TABLE "goodreadsBooks" ADD PRIMARY KEY ("Uuid");
