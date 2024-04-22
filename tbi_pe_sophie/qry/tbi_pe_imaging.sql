WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
		-- NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_10_CODE,
		-- NOMENCLATURE.SOURCE_STRING AS ICD_10_DESC
	FROM
		DIAGNOSIS,
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		NOMENCLATURE
	WHERE
	    ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			-- 3796, -- HC Childrens
			-- 3821, -- HH Clinics
			3822, -- HH Trans Care
			3823 -- HH Rehab		
		)
		AND ENCOUNTER.DISCH_DT_TM BETWEEN
			DECODE(
				@Prompt('Choose date range', 'A', {'Last Week', 'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), 'America/Chicago'),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'03/01/2019 00:00:00'}, User:1),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, 'America/Chicago')
				)
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Last Week', 'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago'),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'03/01/2024 00:00:00'}, User:2),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400,
					pi_time_zone(1, 'America/Chicago')
				)
			)	
		AND ENCOUNTER.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.MED_SERVICE_CD IN (
			9216, -- Neurology
			9217 -- Neurosurgery
		)
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		-- AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^S06|^I61') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
), SCANS AS (
	SELECT DISTINCT
		PATIENTS.*,
		pi_get_cv_display(ORDERS.CATALOG_CD) AS SCAN
	FROM
		ENCNTR_LOC_HIST,
		ORDERS,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = ORDERS.ENCNTR_ID
		AND PATIENTS.PERSON_ID = ORDERS.PERSON_ID
		AND ORDERS.CATALOG_CD IN (
			10161514, -- Abdomen w/wo contrast MRA
			10031431, -- Abdomen w/wo contrast MRI
			153664499, -- Abdomen w/wo contrast MRV		
			1108566, -- Abdomen w/wo IV contrast CT
			10161249, -- Abdomen wo contrast MRA
			10031442, -- Abdomen wo contrast MRI
			153664242, -- Abdomen wo contrast MRV
			1108574, -- Abdomen wo IV contrast CT
			73286694, -- Abdomen/Pelvis CTA
			3111259425, -- Abdomen/Pelvis GI Bleed CTA
			3634829123, -- Abdomen/Pelvis post EVAR CTA
			16945645, -- Abdomen/Pelvis w contrast MRI
			16081130, -- Abdomen/Pelvis w IV contrast CT
			16945765, -- Abdomen/Pelvis w/wo contrast MRI
			1108566, -- Abdomen w/wo IV contrast CT
			10031442, -- Abdomen wo contrast MRI
			1108574, -- Abdomen wo IV contrast CT
			384309162, -- Chest/Abdomen CTA
			9849777 -- Chest CTA
		)
		AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
		AND ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
				AND ELH.ACTIVE_IND = 1
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= ORDERS.ORIG_ORDER_DT_TM
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND ENCNTR_LOC_HIST.MED_SERVICE_CD IN (
			9216, -- Neurology
			9217 -- Neurosurgery		
		)
), PE_PTS AS (
	SELECT DISTINCT
		SCANS.ENCNTR_ID,
		'TRUE' AS PE_ICD10
		-- NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_10_CODE,
		-- NOMENCLATURE.SOURCE_STRING AS ICD_10_DESC
	FROM
		DIAGNOSIS,
		NOMENCLATURE,
		SCANS
	WHERE
		SCANS.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		-- AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I26') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
), DOSES AS (
	SELECT DISTINCT
		SCANS.ENCNTR_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		'TRUE' AS ANTICOAG
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		SCANS
	WHERE
		SCANS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND SCANS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37557146, -- heparin
			37556844, -- enoxaparin		
			37558355, -- warfarin
			535736194, -- dabigatran
			642177890, -- rivaroxaban
			894197564 -- apixaban
			-- 1466817862 -- edoxaban
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (
			(CLINICAL_EVENT.EVENT_CD = 37557146 AND CE_MED_RESULT.IV_EVENT_CD = 688706)
			OR (CLINICAL_EVENT.EVENT_CD = 37556844 AND CE_MED_RESULT.ADMIN_DOSAGE > 40)
			OR (CLINICAL_EVENT.EVENT_CD NOT IN (37557146, 37556844))
		)
), DOSES_PIVOT AS (
	SELECT * FROM DOSES
	PIVOT(
		MIN(ANTICOAG) FOR MEDICATION IN (
			'heparin' AS HEPARIN,
			'enoxaparin' AS ENOXAPARIN,
			'warfarin' AS WARFARIN,
			'dabigatran' AS DABIGATRAN,
			'apixaban' AS APIXABAN,
			'rivaroxaban' AS RIVAROXABAN
		)
	)
)

SELECT DISTINCT
	SCANS.ENCNTR_ID,
	ENCNTR_ALIAS.ALIAS AS FIN,
	PE_PTS.PE_ICD10,
	TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE,
	pi_get_cv_display(PERSON.SEX_CD) AS SEX,
	pi_get_cv_display(PERSON.RACE_CD) AS RACE,
	pi_get_cv_display(PERSON.ETHNIC_GRP_CD) AS ETHNICITY,
	pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
	pi_from_gmt(ENCOUNTER.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
	DOSES_PIVOT.HEPARIN,
	DOSES_PIVOT.ENOXAPARIN,
	DOSES_PIVOT.WARFARIN,
	DOSES_PIVOT.APIXABAN,
	DOSES_PIVOT.RIVAROXABAN,
	DOSES_PIVOT.DABIGATRAN
	-- SCANS.ICD_10_CODE,
	-- SCANS.ICD_10_DESC,
	-- PE_PTS.ICD_10_CODE AS PE_ICD_CODE,
	-- PE_PTS.ICD_10_DESC AS PE_ICD_DESC
FROM
	DOSES_PIVOT,
	ENCNTR_ALIAS,
	ENCOUNTER,
	PERSON,
	PE_PTS,
	SCANS
WHERE
	SCANS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
	AND SCANS.PERSON_ID = PERSON.PERSON_ID
	AND SCANS.ENCNTR_ID = PE_PTS.ENCNTR_ID(+)
	AND SCANS.ENCNTR_ID = DOSES_PIVOT.ENCNTR_ID(+)
	AND SCANS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1


/* 	AND CLINICAL_EVENT.EVENT_CD IN (
		10161815, -- Abdomen w/wo contrast MRA
		10034173, -- Abdomen with and without contrast MRI
		156933954, -- Abdomen w/wo contrast MRV
		1140849, -- Abdomen w/wo contrast CT
		10161814, -- Abdomen w/o contr MRA
		10034175, -- Abdomen without contrast MRI
		156933953, -- Abdomen w/o contrast MRV
		1140850, -- Abdomen wo contrast CT
		-- Abd/Pelv CTA
		3111362511, -- Abdomen/Pelvis GI Bleed CTA
		3641943113, -- Abdomen/Pelvis post EVAR CTA
		16961958, -- Abdomen/Pelvis w contrast MRI
		16084193, -- Abdomen/Pelvis w contrast CT
		16961959, -- Abdomen/Pelvis w/wo contrast MRI
		-- Abd/Pelv w/wo CT
		16961960, -- Abdomen/Pelvis wo contrast MRI
		50728906, -- Abdomen/Pelvis wo contrast CT
		386209004, -- Abdomen and Chest CTA
		9851881, -- Chest CTA
		
	) */