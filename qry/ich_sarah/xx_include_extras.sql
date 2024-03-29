WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCOUNTER.ARRIVE_DT_TM,
		ENCOUNTER.REG_DT_TM,
		ENCOUNTER.DISCH_DT_TM,
		ENCOUNTER.ADMIT_SRC_CD,
		pi_get_cv_display(ENCOUNTER.ADMIT_SRC_CD) AS ADMIT_SRC,
		pi_get_cv_display(ENCOUNTER.DISCH_DISPOSITION_CD) AS DISCH_DISPOSITION
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), TFR_FACILITY AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		PATIENTS.PERSON_ID,
		ENCOUNTER.ENCNTR_ID AS TFR_ENCNTR_ID,
		ENCOUNTER.ARRIVE_DT_TM,
		ENCOUNTER.REG_DT_TM,
		ENCOUNTER.DISCH_DT_TM,
		-- pi_from_gmt(ENCOUNTER.ARRIVE_DT_TM, 'America/Chicago') AS TFR_ARRIVE_DATETIME,
		pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS TFR_FACILITY,
		pi_get_cv_display(ENCOUNTER.ADMIT_SRC_CD) AS TFR_ADMIT_SRC,
		pi_get_cv_display(ENCOUNTER.DISCH_DISPOSITION_CD) AS TFR_DISCH_DISPOSITION,
		pi_get_cv_display(ENCOUNTER.ENCNTR_TYPE_CLASS_CD) AS TFR_ENCNTR_TYPE
	FROM
		ENCOUNTER,
		PATIENTS
	WHERE
		PATIENTS.ADMIT_SRC_CD IN (
			9065, -- Tfr/Hosp
			9066, -- TFR/SNF
			8699127, -- Transfer from Distinct Unit
			14898275, -- ER TFR from SNF
			57312027, -- TFR/Other Facility
			240354718, -- TFR From Hospice
			240354924, -- ER TFR from Hospital
			240355231 -- ER Tfr Fr Other Fac
		)
		AND PATIENTS.PERSON_ID = ENCOUNTER.PERSON_ID
		AND ENCOUNTER.DISCH_DT_TM BETWEEN PATIENTS.REG_DT_TM - 1 AND PATIENTS.REG_DT_TM
		AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD IN (
			42631, -- Inpatient
			55851, -- Emergency
			688523 -- Observation
		)
), PREGNANT AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.RESULT_VAL AS PREGNANT
		-- 'TRUE' AS PREGNANT
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			33325, -- S Preg
			34136 -- U Preg
		)
		AND CLINICAL_EVENT.RESULT_VAL <> 'Negative'
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
), ENCNTR_LIST AS (
	SELECT ENCNTR_ID FROM PATIENTS
	UNION
	SELECT TFR_ENCNTR_ID AS ENCNTR_ID FROM TFR_FACILITY
), SBP AS (
    SELECT DISTINCT
        PATIENTS.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		CLINICAL_EVENT.EVENT_ID,
        TO_NUMBER(REGEXP_REPLACE(CLINICAL_EVENT.RESULT_VAL, '>|<')) AS RESULT_VAL
    FROM
        CLINICAL_EVENT,
        PATIENTS
    WHERE
        CLINICAL_EVENT.ENCNTR_ID IN (SELECT ENCNTR_ID FROM ENCNTR_LIST)
        AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
        AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
        AND CLINICAL_EVENT.EVENT_CD IN (
            30098, -- Systolic Blood Pressure
            134401648 -- Arterial Systolic BP 1
        )
        AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN PATIENTS.ARRIVE_DT_TM - 0.5 AND PATIENTS.ARRIVE_DT_TM + 0.25
        AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), SBP_VALS AS (
	SELECT
		ENCNTR_ID,
		MIN(RESULT_VAL) AS MIN_SBP,
		MIN(RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY EVENT_END_DT_TM, EVENT_ID) AS FIRST_SBP
	FROM SBP
	WHERE RESULT_VAL >= 40
    GROUP BY ENCNTR_ID
)

SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	PATIENTS.ENCNTR_ID,
	pi_from_gmt(PATIENTS.ARRIVE_DT_TM, 'America/Chicago') AS ARRIVE_DATETIME,
	pi_from_gmt(PATIENTS.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
	pi_from_gmt(PATIENTS.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
	PATIENTS.DISCH_DT_TM - PATIENTS.REG_DT_TM AS LOS,
	PATIENTS.ADMIT_SRC,
	PATIENTS.DISCH_DISPOSITION,
	TFR_FACILITY.TFR_ENCNTR_ID,
	pi_from_gmt(TFR_FACILITY.ARRIVE_DT_TM, 'America/Chicago') AS TFR_ARRIVE_DATETIME,
	pi_from_gmt(TFR_FACILITY.REG_DT_TM, 'America/Chicago') AS TFR_ADMIT_DATETIME,
	pi_from_gmt(TFR_FACILITY.DISCH_DT_TM, 'America/Chicago') AS TFR_DISCH_DATETIME,
	TFR_FACILITY.DISCH_DT_TM - TFR_FACILITY.REG_DT_TM AS TFR_LOS,
	PATIENTS.ARRIVE_DT_TM - TFR_FACILITY.ARRIVE_DT_TM AS OSH_TO_TMC_TFR,
	TFR_FACILITY.TFR_FACILITY,
	TFR_FACILITY.TFR_ADMIT_SRC,
	TFR_FACILITY.TFR_DISCH_DISPOSITION,
	TFR_FACILITY.TFR_ENCNTR_TYPE,
	SBP_VALS.MIN_SBP,
	SBP_VALS.FIRST_SBP,
	CASE
		WHEN SBP_VALS.MIN_SBP < 100 THEN 'TRUE'
		WHEN SBP_VALS.FIRST_SBP < 150 THEN 'TRUE'
		ELSE 'FALSE'
	END AS EXCL_SBP,
	COALESCE(PREGNANT.PREGNANT, 'FALSE') AS EXCL_PREGNANT,
	CASE
		WHEN PATIENTS.ADMIT_SRC IN ('Clinic or Physician Office Referral', 'Emergency Room', 'ER TFR FR Clinic', 'Physician Referral') THEN 'FALSE'
		WHEN TFR_FACILITY.TFR_FACILITY IN ('TR TIRR', 'GH Rehab', 'HH Rehab', 'HH Trans Care', 'KR Katy Rehab', 'SE REHAB') THEN 'FALSE'
		WHEN TFR_FACILITY.DISCH_DT_TM - TFR_FACILITY.REG_DT_TM <= 0.5 THEN 'FALSE'
		ELSE 'TRUE'
	END AS EXCL_TRANSFER,
	CASE 
		WHEN PATIENTS.DISCH_DT_TM - PATIENTS.REG_DT_TM < 2 AND PATIENTS.DISCH_DISPOSITION IN ('Cadaver Organ Donor', 'Deceased', 'Expired/Donor', 'Hospice-Home', 'Hospice-Medical Facility') THEN 'TRUE' 
		ELSE 'FALSE'
	END AS EXCL_EARLY_DEATH
FROM
	ENCNTR_ALIAS,
	PATIENTS,
	PREGNANT,
	SBP_VALS,
	TFR_FACILITY
WHERE
	PATIENTS.ENCNTR_ID = TFR_FACILITY.ENCNTR_ID(+)
	AND PATIENTS.PERSON_ID = TFR_FACILITY.PERSON_ID(+)
	AND PATIENTS.ENCNTR_ID = PREGNANT.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = SBP_VALS.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1	
