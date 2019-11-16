# ZCL_ALV_TABLE_CREATE_RTTC
Variant of CL_ALV_TABLE_CREATE, to create a table via RTTC instead of GENERATE SUBROUTINE POOL

The method CREATE_DYNAMIC_TABLE can be called 36 times maximum, this is a limit of the ABAP statement GENERATE SUBROUTINE POOL.

There's no such a limit with ZCL_ALV_TABLE_CREATE_RTTC, because it's based on RTTC.

Use it exactly the same way as `cl_alv_table_create=>create_dynamic_table`. For example :

      zcl_alv_table_create_rtts=>create_dynamic_table(
            EXPORTING
              it_fieldcatalog           = lt_fieldcat_lvc
            IMPORTING
              ep_table                  = grt_std
            EXCEPTIONS
              generate_subpool_dir_full = 1
              OTHERS                    = 2
            ).
            
Note: there are also 3 standard function modules which call the static methods of CL_ALV_TABLE_CREATE (REUSE_ALV_CREATE_TABLE, ALV_CREATE_TABLE, LVC_CREATE_TABLE), so you might replace their calls with the static methods of ZCL_ALV_TABLE_CREATE_RTTC.
