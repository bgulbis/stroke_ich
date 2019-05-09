SELECT DISTINCT
    ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID,
	CV_ENCOUTER_TYPE.DISPLAY AS ENCOUNTER_TYPE,
	TO_CHAR(pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ADMIT_DATETIME,
	TO_CHAR(pi_from_gmt(ENCOUNTER.DISCH_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS DISCHARGE_DATETIME,
	TO_CHAR(pi_from_gmt(ENCOUNTER.ARRIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ARRIVE_DATETIME,
    ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,
    TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) AS AGE,
    CV_RACE.DISPLAY AS RACE,
    CV_SEX.DISPLAY AS SEX,
    CV_LOC_NURSE_UNIT.DISPLAY AS NURSE_UNIT_ADMIT,
    CV_MED_SVC.DISPLAY AS MED_SERVICE_DC,
    CV_DISPO.DISPLAY AS DISPO_DC
FROM
    CODE_VALUE CV_DISPO,
	CODE_VALUE CV_ENCOUTER_TYPE,
    CODE_VALUE CV_LOC_NURSE_UNIT,
	CODE_VALUE CV_MED_SVC,
	CODE_VALUE CV_RACE,
	CODE_VALUE CV_SEX,
    DIAGNOSIS,
    ENCNTR_LOC_HIST,
    ENCOUNTER,
    NOMENCLATURE,
    PERSON
WHERE
    NOMENCLATURE.SOURCE_IDENTIFIER IN (
        'I61.0',
        'I61.1',
        'I61.2',
        'I61.3',
        'I61.4',
        'I61.5',
        'I61.6',
        'I61.8',
        'I61.9'
    )
    AND NOMENCLATURE.ACTIVE_IND = 1
    AND (
        NOMENCLATURE.NOMENCLATURE_ID = DIAGNOSIS.NOMENCLATURE_ID
        AND DIAGNOSIS.ACTIVE_IND = 1
        AND DIAGNOSIS.DIAG_TYPE_CD IN (26242, 26243, 26244, 26246)
    )
    AND (
	    DIAGNOSIS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
        AND ENCOUNTER.ACTIVE_IND = 1
    	AND ENCOUNTER.LOC_FACILITY_CD = 3310
    	AND ENCOUNTER.ENCNTR_TYPE_CD IN (29532, 29540)
    	AND ENCOUNTER.DISCH_DISPOSITION_CD = CV_DISPO.CODE_VALUE
		AND ENCOUNTER.ENCNTR_TYPE_CD = CV_ENCOUTER_TYPE.CODE_VALUE
		AND ENCOUNTER.MED_SERVICE_CD = CV_MED_SVC.CODE_VALUE
    )
    AND (
        DIAGNOSIS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
        AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
        AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD IN (283905037, 193270)
        AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
            SELECT MIN(ELH.TRANSACTION_DT_TM)
            FROM ENCNTR_LOC_HIST ELH
            WHERE 
                DIAGNOSIS.ENCNTR_ID = ELH.ENCNTR_ID
                AND ELH.ACTIVE_IND = 1
                AND ELH.LOC_NURSE_UNIT_CD NOT IN (
                    9996629, 
                    277570335, 
                    277573736, 
                    508618957, 
                    2146281534, 
                    9763063, 
                    277567038, 
                    24569260,
                    43683126
                )
        )
        AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = CV_LOC_NURSE_UNIT.CODE_VALUE
    )
    AND (
        ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
        AND PERSON.ACTIVE_IND = 1
        AND PERSON.RACE_CD = CV_RACE.CODE_VALUE
        AND PERSON.SEX_CD = CV_SEX.CODE_VALUE
    )
	AND (
		ENCOUNTER.DISCH_DT_TM + 0
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
		AND ENCOUNTER.DISCH_DT_TM
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
