SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
    CLINICAL_EVENT.EVENT_ID AS EVENT_ID,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS LAB_DATETIME,
	CV_EVENT.DISPLAY AS LAB,
	CLINICAL_EVENT.RESULT_VAL AS RESULT,
	CV_RESULT_UNITS.DISPLAY AS RESULT_UNIT
FROM
    CLINICAL_EVENT,
    CODE_VALUE CV_EVENT,
    CODE_VALUE CV_RESULT_UNITS,
	ENCNTR_ALIAS,
	ENCOUNTER,
	(
        SELECT DISTINCT ENCOUNTER.ENCNTR_ID
        FROM ENCOUNTER, CLINICAL_EVENT
        WHERE 
        	CLINICAL_EVENT.EVENT_CD IN (363180766, 423546852)
        	AND (
        		CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
        		AND CLINICAL_EVENT.PERSON_ID = ENCOUNTER.PERSON_ID
        		AND ENCOUNTER.ACTIVE_IND = 1
        		AND ENCOUNTER.LOC_FACILITY_CD IN (3310, 3796, 3821, 3822, 3823)
        	)
        	AND (
        		CLINICAL_EVENT.EVENT_END_DT_TM + 0
        			BETWEEN DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
        				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
        				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Month to Date', pi_to_gmt(TRUNC(SYSDATE - 1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
        				'User-defined', pi_to_gmt(
        					TO_DATE(
        						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
        						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
        					),
        					pi_time_zone(1, @Variable('BOUSER'))),
        				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), pi_time_zone(2, @Variable('BOUSER')))
        			)
        			AND DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'User-defined', pi_to_gmt(
        					TO_DATE(
        						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
        						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
        					),
        					pi_time_zone(1, @Variable('BOUSER'))),
        				'N Days Prior', pi_to_gmt(SYSDATE, pi_time_zone(2, @Variable('BOUSER')))
        			)
        		AND CLINICAL_EVENT.EVENT_END_DT_TM
        			BETWEEN DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', TRUNC(SYSDATE),
        				'Yesterday', TRUNC(SYSDATE) - 1,
        				'Week to Date', TRUNC(SYSDATE, 'DAY'),
        				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
        				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
        				'Month to Date', TRUNC(SYSDATE - 1, 'MONTH'),
        				'User-defined', DECODE(
        					@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
        					'01/01/1800 00:00:00',
        					'',
        					@Variable('Enter begin date (Leave as 01/01/1800 if using a Relative Date)')
        				),
        				'N Days Prior', SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {0}, User:2080)
        			) - 1
        			AND DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', TRUNC(SYSDATE) + (86399 / 86400),
        				'Yesterday', TRUNC(SYSDATE) - (1 / 86400),
        				'Week to Date', TRUNC(SYSDATE) - (1 / 86400),
        				'Last Week', TRUNC(SYSDATE, 'DAY') - (1 / 86400),
        				'Last Month', TRUNC(SYSDATE, 'MONTH') - (1 / 86400),
        				'Month to Date', TRUNC(SYSDATE) - (1 / 86400),
        				'User-defined', DECODE(
        					@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
        					'01/01/1800 00:00:00',
        					'',
        					@Variable('Enter end date (Leave as 01/01/1800 if using a Relative Date)')
        				),
        				'N Days Prior', SYSDATE
        			) + 1
        	)
        
        MINUS
        
        SELECT DISTINCT ENCOUNTER.ENCNTR_ID
        FROM ENCOUNTER, ORDERS
        WHERE 
        	ORDERS.ACTIVE_IND = 1
        	AND ORDERS.CATALOG_CD = 363097401
            AND ORDERS.CATALOG_TYPE_CD = 1363
            AND ORDERS.ORIG_ORD_AS_FLAG = 2
            AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
            AND ORDERS.ORIG_ORDER_DT_TM BETWEEN DATE '2009-12-31' AND DATE '2019-01-02'
        	AND (
        		ORDERS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
                AND ORDERS.PERSON_ID = ENCOUNTER.PERSON_ID
        		AND ENCOUNTER.ACTIVE_IND = 1
                AND ENCOUNTER.LOC_FACILITY_CD IN (3310, 3796, 3821, 3822, 3823)
            )
	) LACOSAMIDE_PTS
WHERE
	LACOSAMIDE_PTS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
	AND (
		ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.END_EFFECTIVE_DT_TM > SYSDATE
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619
	)
    AND (
    	ENCOUNTER.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
        AND CLINICAL_EVENT.EVENT_CD IN (
            31550, 
            33022, 
            33010,
            31551,
            34316,
            31489,
            32384,
            31502,
            33553,
            481004437,
            31602,
            32654,
            821906799,
            32207,
            9485085,
            33096,
            114733,
            9485086,
            821906947,
            2252391613
        )
	    AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
        AND CLINICAL_EVENT.EVENT_CD = CV_EVENT.CODE_VALUE
        AND CLINICAL_EVENT.RESULT_UNITS_CD = CV_RESULT_UNITS.CODE_VALUE
        AND CLINICAL_EVENT.RESULT_VAL IS NOT NULL
    )
