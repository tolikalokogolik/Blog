--- 1.

SELECT COUNT(DISTINCT AgreementGenId) 
FROM `huhl-course.SWEDBANK.defaults`

--- 3.

WITH tbl1 AS (
  SELECT AgreementGenId,
         DefaultDate,
         DefaultEndDate,
         DefaultRankNum,
         ValutaKod,
         LossAmount,
         EAD,
         LAG(DefaultEndDate,1, '0001-01-01') OVER (ORDER BY AgreementGenId) AS lag_day
  FROM huhl-course.SWEDBANK.defaults
), tbl2 AS (
  SELECT *, 
         CASE WHEN DATE_DIFF(lag_day, DefaultDate, MONTH) < 9 THEN 1 ELSE -0 END AS gflag
  FROM tbl1 
),tbl3 AS (
  SELECT *,
         SUM(CASE WHEN gflag=0 THEN 1 ELSE 0 END) OVER(ORDER BY AgreementGenId) AS gid
  FROM tbl2
)
SELECT MIN(tbl3.AgreementGenId) AS AgreementGenId,
       MIN(DefaultDate) AS DefaultDate,
       MAX(DefaultEndDate) AS DefaultEndDate,
       MIN(tbl3.DefaultRankNum) AS DefaultRankNum,
       ANY_VALUE(default_type.DefaultTypeCd) AS DefaultTypeCd,
       ANY_VALUE(tbl3.ValutaKod) AS ValutaKod,
       MIN(LossAmount) AS LossAmount,
       MIN(EAD) AS EAD
FROM tbl3 
  LEFT JOIN `huhl-course.SWEDBANK.default_types` AS default_type
  ON tbl3.DefaultRankNum = default_type.DefaultRankNum
GROUP BY gid;

--- 4. 

SELECT COUNT(DISTINCT AgreementGenId) 
FROM `huhl-course.SWEDBANK.defaults_compressed`;

--- 6.

WITH tbl1 AS (SELECT *,
       CASE WHEN DefaultEndDate > CURRENT_DATE() THEN 0
        ELSE 1 END AS closed 
FROM `huhl-course.SWEDBANK.defaults`)
SELECT any_value(closed) AS IsClosed,
       COUNT(*) AS total
FROM tbl1 GROUP BY closed;

--- 7.

WITH tbl1 AS (
  SELECT
    LossInSEK,
    LossInSEK+0.016*EADInSEK AS NewLossInSEK,
    EADInSEK,
    DefaultTypeCd
  FROM `huhl-course.SWEDBANK.defaultsWithSEK`
)
SELECT SUM(LossInSEK) AS SumLossInSEK,
       SUM(NewLossInSEK) AS NewSumLossInSEK
FROM tbl1;

WITH tbl1 AS (
  SELECT
    LossInSEK,
    LossInSEK+0.016*EADInSEK AS NewLossInSEK,
    EADInSEK,
    DefaultTypeCd
  FROM `huhl-course.SWEDBANK.defaultsWithSEK`
)
SELECT DefaultTypeCd,
       SUM(NewLossInSEK)/SUM(EADInSEK) AS Percentage
FROM tbl1 GROUP BY DefaultTypeCd
UNION ALL
SELECT 'ALL' AS DefaultTypeCd,
       SUM(NewLossInSEK)/SUM(EADInSEK) AS Percentage
FROM tbl1;
