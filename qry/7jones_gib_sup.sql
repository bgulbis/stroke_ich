WITH DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ROUTE
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
		-- ORDERS
	WHERE
		CLINICAL_EVENT.EVENT_CD IN (
			37556891, -- esomeprazole
			37557397, -- lansoprazole
			37557721, -- omeprazole
			37557761, -- pantoprazole
			37557958, -- rabeprazole
			359088163, -- dexlansoprazole
			51208912 -- famotidine
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'10/01/2020 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'10/01/2024 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'America/Chicago'
			)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
				AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 283905037 -- HH 7J
		-- AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
)

SELECT * FROM DOSES