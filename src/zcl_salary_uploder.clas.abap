CLASS zcl_salary_uploder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
         wa_je_deep LIKE LINE OF lt_je_deep,
         w2         LIKE LINE OF wa_je_deep-%param-_withholdingtaxitems,
         w3         LIKE LINE OF w2-_currencyamount.

    TYPES : BEGIN OF ty_payl,
              srno                         TYPE i,
              employeecode                 TYPE lifnr,
              employee_name                TYPE i_supplier-suppliername,
              department(40)               TYPE c,
              salary                       TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              wages                        TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              stipend                      TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              goodwork                     TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              pf                           TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              esi                          TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              advance                      TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              loan                         TYPE  i_operationalacctgdocitem-amountintransactioncurrency,

              loaninterest                 TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              electric                     TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              idcard_det                   TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              darided                      TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              otherexp                     TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              otherded                     TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              roomded                      TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              uniformexp                   TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              userdefine01                 TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              userdefine02                 TYPE  i_operationalacctgdocitem-amountintransactioncurrency,
              pt                           TYPE  i_operationalacctgdocitem-amountintransactioncurrency,

              taxtype                      LIKE w2-withholdingtaxtype,
              taxcode                      LIKE w2-withholdingtaxcode,
              tdsbase                      LIKE w3-taxbaseamount,
              tdsamount                    LIKE w3-taxamount,
              net                          TYPE i_operationalacctgdocitem-amountintransactioncurrency,
              currency                     TYPE i_operationalacctgdocitem-transactioncurrency,
              postingdate(10)              TYPE c,
              documentdate(10)             TYPE c,
              companycode                  TYPE bukrs,
              documentreferenceid          TYPE i_journalentrytp-documentreferenceid,
              accountingdocumenttype       TYPE blart,
              accountingdocumentheadertext TYPE bktxt,
              assignment(40)               TYPE c,
              businessplace                TYPE i_operationalacctgdocitem-businessplace,
              costcenter                   TYPE kostl,
              profitcenter                 TYPE prctr,
              housebank                    TYPE string,
              housebankid                  TYPE string,
              glaccount(10)                TYPE c,
            END OF ty_payl.

    CLASS-DATA:
      lv_cid     TYPE abp_behv_cid,
      it_pay     TYPE TABLE OF ty_payl,
      wa_pay     TYPE ty_payl,
      i_responce TYPE TABLE OF string.

    CLASS-DATA:doc       TYPE string,
               error     TYPE string,
               responce1 TYPE string,
               item(6)   TYPE n,
               item1(6)  TYPE n.

    CLASS-DATA:gl_item        LIKE wa_je_deep-%param-_glitems[],
               ar_item        LIKE wa_je_deep-%param-_aritems[],
               ap_item        LIKE wa_je_deep-%param-_apitems[],
               with_hold_item LIKE wa_je_deep-%param-_withholdingtaxitems[].

    CLASS-DATA:BEGIN OF i_gl,
                 gl_code            TYPE string,
                 goods_ap_gl_code   TYPE string,
                 goods_exp_gl_code  TYPE string,
                 advance_ap_gl_code TYPE string,
               END OF i_gl.

    METHODS:modify_item
      IMPORTING employeecode           TYPE string OPTIONAL
                glcode                 TYPE string
                ap_glcode              TYPE string OPTIONAL
                journalentryitemamount TYPE i_operationalacctgdocitem-amountintransactioncurrency
                net                    TYPE i_operationalacctgdocitem-amountintransactioncurrency OPTIONAL
                gl_flag                TYPE c OPTIONAL
                ap_flag                TYPE c OPTIONAL
                sp_gl                  TYPE c OPTIONAL
                credit_debit           TYPE string
                item_text              TYPE string OPTIONAL.


    METHODS:modify_glwise_ap_item
      IMPORTING employeecode           TYPE string OPTIONAL
*                glcode                 TYPE string OPTIONAL
                ap_glcode              TYPE string OPTIONAL
                journalentryitemamount TYPE i_operationalacctgdocitem-amountintransactioncurrency
*                credit_debit           TYPE string
                specialglcode          TYPE c OPTIONAL
                item_text              TYPE string OPTIONAL.

    METHODS:modify_serial_no_wise.
    METHODS:modify_gl_wise.
    METHODS:modify_with_holding_tax
      IMPORTING taxcode TYPE string.

    TYPES:BEGIN OF ty_supplier,
            supplier(10) TYPE c,
            gl_code(10)  TYPE c,
          END OF ty_supplier.
    DATA:i_supplier TYPE TABLE OF ty_supplier.

    DATA:it_head LIKE it_pay,
         wa_head LIKE LINE OF it_head.

    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SALARY_UPLODER IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.



    DATA(req) = request->get_form_fields(  ).
    response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
    response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).


    DATA(body)  = request->get_text(  )  .
    xco_cp_json=>data->from_string( body )->write_to( REF #(  it_pay ) ).

    TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.

    LOOP AT it_pay ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-employeecode = |{ <fs>-employeecode   ALPHA = IN }|.
      <fs>-costcenter   = |{ <fs>-costcenter ALPHA = IN }|.
      <fs>-profitcenter = |{ <fs>-profitcenter ALPHA = IN }|.
    ENDLOOP.

    DATA:count TYPE i.
    LOOP AT it_pay ASSIGNING <fs> WHERE glaccount IS NOT INITIAL.
      count = count + 1.
      EXIT.
    ENDLOOP.
    LOOP AT it_pay ASSIGNING <fs> WHERE glaccount IS INITIAL.
      count = count + 1.
      EXIT.
    ENDLOOP.
    IF count GT 1.
      DATA(message) = 'GL Field Should Not Be Blank'.
    ENDIF.

    IF message IS INITIAL.
      it_head = it_pay[].

      READ TABLE it_head INTO wa_head INDEX 1.
      IF wa_head-glaccount IS INITIAL.
        SORT it_head BY srno.
        DELETE ADJACENT DUPLICATES FROM it_head COMPARING srno.
        SORT it_pay STABLE BY srno . "glaccountlineitem.
      ELSE.
        SORT it_head BY glaccount.
        DELETE ADJACENT DUPLICATES FROM it_head COMPARING glaccount.
        SORT it_pay STABLE BY srno . "glaccountlineitem.
      ENDIF.


      CLEAR:gl_item , ar_item , ap_item , with_hold_item , doc , error , responce1.

      IF it_pay IS NOT INITIAL.

        SELECT FROM i_suppliercompany AS a
        FIELDS a~supplier ,
               a~reconciliationaccount AS gl_code
        FOR ALL ENTRIES IN @it_pay
        WHERE supplier = @it_pay-employeecode
        INTO TABLE @i_supplier.
        SORT i_supplier BY supplier.
        SORT it_pay ASCENDING STABLE BY tdsamount.

      ENDIF.



      LOOP AT it_head INTO wa_head.

        CLEAR item.
        DATA(posting_dt)  = wa_head-postingdate+0(4)  && wa_head-postingdate+5(2)  && wa_head-postingdate+8(2).
        DATA(document_dt) = wa_head-documentdate+0(4) && wa_head-documentdate+5(2) && wa_head-documentdate+8(2).

        wa_je_deep-%cid   = lv_cid.
        wa_je_deep-%param = VALUE #( companycode                  = wa_head-companycode
                                     documentreferenceid          = wa_head-documentreferenceid
                                     createdbyuser                = 'Salary uploader program'
                                     businesstransactiontype      = 'RFBU'
                                     accountingdocumenttype       = wa_head-accountingdocumenttype
                                     documentdate                 = document_dt
                                     postingdate                  = posting_dt
                                     accountingdocumentheadertext = wa_head-accountingdocumentheadertext ).

        IF wa_head-glaccount IS INITIAL.
          me->modify_serial_no_wise(  ).
        ELSE.
          me->modify_gl_wise(  ).
        ENDIF.

        APPEND wa_je_deep TO lt_je_deep.
        CLEAR:wa_je_deep,gl_item,ap_item,ar_item,wa_pay .

        MODIFY ENTITIES OF i_journalentrytp
        ENTITY journalentry
        EXECUTE post FROM lt_je_deep
        FAILED DATA(ls_failed_deep)
        REPORTED DATA(ls_reported_deep)
        MAPPED DATA(ls_mapped_deep).



        IF ls_failed_deep IS NOT INITIAL.
          LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
            IF sy-tabix <> 1.
              IF <ls_reported_deep>-%msg->if_t100_dyn_msg~msgty = 'E'.
                DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_longtext( ).
                CONCATENATE '$$$$ Error :-' lv_result INTO responce1 .
                APPEND responce1 TO i_responce.
                CLEAR responce1.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          COMMIT ENTITIES BEGIN
          RESPONSE OF i_journalentrytp
          FAILED DATA(lt_commit_failed)
          REPORTED DATA(lt_commit_reported).
          ...
          COMMIT ENTITIES END.
          LOOP AT lt_commit_reported-journalentry INTO DATA(w).
            IF w-%msg->if_t100_dyn_msg~msgty = 'S'.
              responce1  = |$$$$ Document :-{ w-%msg->if_t100_dyn_msg~msgv2+0(10) } Generated|.
              APPEND responce1 TO i_responce.
              CLEAR responce1.
            ENDIF.
          ENDLOOP.
        ENDIF.

        CLEAR:wa_je_deep,gl_item,ap_item,ar_item,lt_je_deep,with_hold_item.
      ENDLOOP.



      DATA:json TYPE REF TO if_xco_cp_json_data.
      CLEAR:responce1.

      xco_cp_json=>data->from_abap(
        EXPORTING
          ia_abap      = i_responce
        RECEIVING
          ro_json_data = json   ).
      json->to_string(
        RECEIVING
          rv_string =  responce1 ).

    ELSE.

      responce1 = message.

    ENDIF.

    response->set_text( responce1 ).

  ENDMETHOD.


  METHOD modify_glwise_ap_item.
    IF journalentryitemamount IS NOT INITIAL.
      wa_head-salary = wa_head-salary + journalentryitemamount.
      item = item + 1.
      ap_item =  VALUE #( (
                            glaccountlineitem  = item
                            glaccount          = ap_glcode
                            supplier           = employeecode
                            businessplace      = wa_pay-businessplace
                            profitcenter       = wa_pay-profitcenter
                            specialglcode      = specialglcode
                            reference1idbybusinesspartner = item_text
                            reference2idbybusinesspartner = wa_pay-employeecode
                            _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                            journalentryitemamount = journalentryitemamount
                                                            currency               = wa_pay-currency      ) ) ) ).
      APPEND LINES OF ap_item  TO wa_je_deep-%param-_apitems.
    ENDIF.

  ENDMETHOD.


  METHOD modify_gl_wise.

    CLEAR:wa_head-salary.

    item1 = item + 1.
    LOOP AT it_pay INTO wa_pay WHERE glaccount = wa_head-glaccount.

      READ TABLE i_supplier INTO DATA(w_supplier) WITH KEY supplier = wa_pay-employeecode
                                                       BINARY SEARCH.
      IF sy-subrc = 0.

        i_gl-gl_code            = COND #( WHEN w_supplier-gl_code = '0001611003' THEN '0004201000'
                                          WHEN w_supplier-gl_code = '0001611008' THEN '0004201001'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0004201002'
                                          WHEN w_supplier-gl_code = '0001611010' THEN '0004201014'
                                          WHEN w_supplier-gl_code = '0001611012' THEN '0004201003' ).

        i_gl-advance_ap_gl_code = COND #( WHEN w_supplier-gl_code = '0001611003' THEN '0002731051'
                                          WHEN w_supplier-gl_code = '0001611008' THEN '0002731051'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0002731050'
                                          WHEN w_supplier-gl_code = '0001611010' THEN '0002731051'
                                          WHEN w_supplier-gl_code = '0001611012' THEN '0002731051' ).

        i_gl-goods_ap_gl_code   = COND #( WHEN w_supplier-gl_code = '0001611008' THEN '0001611011'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0001611004' ).

        i_gl-goods_exp_gl_code  = COND #( WHEN w_supplier-gl_code = '0001611008' THEN '0004201005'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0004201004' ).

      ENDIF.

*      wa_head-salary = wa_head-salary + wa_pay-salary.

      me->modify_glwise_ap_item( employeecode           = CONV #( wa_pay-employeecode )
                          journalentryitemamount = wa_pay-salary
                          item_text              = 'Salary' ).

      me->modify_glwise_ap_item( employeecode           = CONV #( wa_pay-employeecode )
                          journalentryitemamount = wa_pay-goodwork
                          specialglcode          = '8'
                          ap_glcode              = i_gl-goods_ap_gl_code
                          item_text              = 'Goodwork'   ).

      me->modify_glwise_ap_item( employeecode           = CONV #( wa_pay-employeecode )
                          journalentryitemamount = wa_pay-advance
                          ap_glcode              = i_gl-advance_ap_gl_code
                          specialglcode          = 'A'
                          item_text              = 'Advance'  ).

      me->modify_glwise_ap_item( employeecode           = CONV #( wa_pay-employeecode )
                          journalentryitemamount = wa_pay-loan
                          ap_glcode              = i_gl-advance_ap_gl_code
                          item_text              = 'Loan'  ).

      me->modify_glwise_ap_item( employeecode           = CONV #( wa_pay-employeecode )
                          journalentryitemamount = wa_pay-loaninterest
                          ap_glcode              = i_gl-advance_ap_gl_code
                          item_text              = 'LoanInterest'  ).

    ENDLOOP.


    item = item + 1.
    gl_item =  VALUE #( ( glaccountlineitem  = item
                          glaccount          = wa_pay-glaccount
                          businessplace      = wa_pay-businessplace
                          costcenter         = wa_pay-costcenter
                          profitcenter       = wa_pay-profitcenter
                          housebank          = wa_pay-housebank
                          housebankaccount   = wa_pay-housebankid

                          _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                          journalentryitemamount = wa_head-salary * -1
                                                          currency               = wa_pay-currency   ) ) ) ).
    APPEND LINES OF gl_item TO wa_je_deep-%param-_glitems.


  ENDMETHOD.


  METHOD modify_item.

    CHECK journalentryitemamount IS NOT INITIAL.

    DATA:amt1 LIKE journalentryitemamount.
    DATA:amt2 LIKE journalentryitemamount.

    IF credit_debit = 'DR'.
      amt1 = journalentryitemamount.
      amt2 = journalentryitemamount * -1.
    ELSE.
      amt1 = journalentryitemamount * -1.
      amt2 = journalentryitemamount .
    ENDIF.


    IF employeecode IS NOT INITIAL.

      IF net IS NOT INITIAL AND amt1 GT 0.
        amt1 = net.
      ELSEIF net IS NOT INITIAL AND amt1 LT 0.
        amt1 = net * -1.
      ENDIF.

      IF ap_flag IS NOT INITIAL.


        item = item + 1.

        ap_item =  VALUE #( (
                              glaccountlineitem  = item
*                              glaccount          = ap_glcode
                              specialglcode      = sp_gl
                              supplier           = employeecode
                              businessplace      = wa_pay-businessplace
                              profitcenter       = wa_pay-profitcenter
                              reference1idbybusinesspartner = item_text
                              reference2idbybusinesspartner = wa_pay-employeecode
                              _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                              journalentryitemamount = amt1
                                                              currency               = wa_pay-currency      ) ) ) ).
        APPEND LINES OF ap_item  TO wa_je_deep-%param-_apitems.
      ENDIF.

      IF gl_flag IS NOT INITIAL.
        item = item + 1.
        gl_item =  VALUE #( ( glaccountlineitem  = item
                              glaccount          = glcode         "    wa_pay-glaccount
                              businessplace      = wa_pay-businessplace
                              costcenter         = wa_pay-costcenter
                              profitcenter       = wa_pay-profitcenter
                              reference1idbybusinesspartner = item_text
                              reference2idbybusinesspartner = wa_pay-employeecode

                              _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                              journalentryitemamount = amt2
                                                              currency               = wa_pay-currency   ) ) ) ).
        APPEND LINES OF gl_item TO wa_je_deep-%param-_glitems.
      ENDIF.

    ELSE.

      item = item + 1.
      gl_item =  VALUE #( ( glaccountlineitem  = item
                            glaccount          = ap_glcode         "    wa_pay-glaccount
                            businessplace      = wa_pay-businessplace
                            costcenter         = wa_pay-costcenter
                            profitcenter       = wa_pay-profitcenter
                            reference1idbybusinesspartner = item_text
                            reference2idbybusinesspartner = wa_pay-employeecode

                            _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                            journalentryitemamount = amt1
                                                            currency               = wa_pay-currency   ) ) ) ).
      APPEND LINES OF gl_item TO wa_je_deep-%param-_glitems.

      IF gl_flag IS NOT INITIAL.
        item = item + 1.
        gl_item =  VALUE #( ( glaccountlineitem  = item
                              glaccount          = glcode         "    wa_pay-glaccount
                              businessplace      = wa_pay-businessplace
                              costcenter         = wa_pay-costcenter
                              profitcenter       = wa_pay-profitcenter
                              reference1idbybusinesspartner = item_text
                              reference2idbybusinesspartner = wa_pay-employeecode

                              _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                              journalentryitemamount = amt2
                                                              currency               = wa_pay-currency   ) ) ) ).
        APPEND LINES OF gl_item TO wa_je_deep-%param-_glitems.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD modify_serial_no_wise.

    LOOP AT it_pay INTO wa_pay WHERE srno = wa_head-srno.

      READ TABLE i_supplier INTO DATA(w_supplier) WITH KEY supplier = wa_pay-employeecode
                                                           BINARY SEARCH.
      IF sy-subrc = 0.

        i_gl-gl_code            = COND #( WHEN w_supplier-gl_code = '0001611003' THEN '0004201000'
                                          WHEN w_supplier-gl_code = '0001611008' THEN '0004201001'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0004201002'
                                          WHEN w_supplier-gl_code = '0001611010' THEN '0004201014'
                                          WHEN w_supplier-gl_code = '0001611012' THEN '0004201003' ).

        i_gl-advance_ap_gl_code = COND #( WHEN w_supplier-gl_code = '0001611003' THEN '0002731051'
                                          WHEN w_supplier-gl_code = '0001611008' THEN '0002731051'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0002731050'
                                          WHEN w_supplier-gl_code = '0001611010' THEN '0002731051'
                                          WHEN w_supplier-gl_code = '0001611012' THEN '0002731051' ).

        i_gl-goods_ap_gl_code   = COND #( WHEN w_supplier-gl_code = '0001611008' THEN '0001611011'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0001611004' ).

        i_gl-goods_exp_gl_code  = COND #( WHEN w_supplier-gl_code = '0001611008' THEN '0004201005'
                                          WHEN w_supplier-gl_code = '0001611009' THEN '0004201004' ).

      ENDIF.

      IF wa_pay-net IS NOT INITIAL AND wa_pay-salary IS NOT INITIAL.
        modify_with_holding_tax( taxcode = CONV #( wa_pay-taxcode ) ).
      ENDIF.

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-salary
                       net                    = wa_pay-net
                       gl_flag                = 'X'
                       ap_flag                = COND #( WHEN wa_pay-net IS INITIAL
                                                        THEN '' ELSE 'X'    ) "'X'
                       credit_debit           = 'CR'
                       item_text              = 'Salary' ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-wages
                       net                    = wa_pay-net
                       gl_flag                = 'X'
                       ap_flag                = COND #( WHEN wa_pay-net IS INITIAL
                                                        THEN '' ELSE 'X'    ) "'X'
                       credit_debit           = 'CR'
                       item_text              = 'Wages' ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-stipend
                       net                    = wa_pay-net
                       gl_flag                = 'X'
                       ap_flag                = COND #( WHEN wa_pay-net IS INITIAL
                                                        THEN '' ELSE 'X'    ) "'X'
                       credit_debit           = 'CR'
                       item_text              = 'Stipend'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-goods_exp_gl_code
                       journalentryitemamount = wa_pay-goodwork
*                       ap_glcode              = i_gl-goods_ap_gl_code
                       sp_gl                  = '8'
                       gl_flag                = 'X'
                       ap_flag                = 'X'
                       credit_debit           = 'CR'
                       item_text              = 'Goodwork'   ).


      me->modify_item( employeecode           = ''
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-pf
                       ap_glcode              = '0001621000'
                       credit_debit           = 'CR'
                       ap_flag                = 'X'
*                       gl_flag                = 'X'
                       item_text              = 'PF'  ).


      me->modify_item( employeecode           = ''
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-esi
                       ap_glcode              = '0001621001'
                       credit_debit           = 'CR'
                       ap_flag                = 'X'
*                       gl_flag                = 'X'
                       item_text              = 'ESI'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-advance
*                       ap_glcode              = i_gl-advance_ap_gl_code
                       sp_gl                  = 'A'
                       credit_debit           = 'CR'
                       ap_flag                = 'X'
                       item_text              = 'Advance'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-loan
*                       ap_glcode              = i_gl-advance_ap_gl_code
                       sp_gl                  = 'A'
                       credit_debit           = 'CR'
                       ap_flag                = 'X'
                       item_text              = 'Loan'  ).


      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-loaninterest
*                       ap_glcode              = i_gl-advance_ap_gl_code
                       sp_gl                  = 'A'
                       credit_debit           = 'CR'
                       ap_flag                = 'X'
                       item_text              = 'LoanInterest'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = '0004411000'
                       journalentryitemamount = wa_pay-electric
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'Electric'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = '0004231000'
                       journalentryitemamount = wa_pay-idcard_det
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'IDCard_Det'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = '0004231000'
                       journalentryitemamount = wa_pay-darided
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'DariDed'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-otherexp
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'OtherExp'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-otherded
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'OtherDed'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-roomded
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'RoomDed'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = '0004231000'
                       journalentryitemamount = wa_pay-uniformexp
                       credit_debit           = 'DR'
                       gl_flag                = 'X'
                       item_text              = 'UniformExp'  ).

      me->modify_item( employeecode           = CONV #( wa_pay-employeecode )
                       glcode                 = i_gl-gl_code
                       journalentryitemamount = wa_pay-userdefine01
*                       ap_glcode              = i_gl-advance_ap_gl_code
                       sp_gl                  = 'H'
                       credit_debit           = 'CR'
                       ap_flag                = 'X'
                       item_text              = 'Vehicle Adv'  ).


    ENDLOOP.
  ENDMETHOD.


  METHOD modify_with_holding_tax.

    CHECK wa_pay-tdsamount IS NOT INITIAL AND wa_pay-taxcode IS NOT INITIAL.

    item1 = item + 1.
    IF wa_pay-tdsamount IS NOT INITIAL.

      with_hold_item =  VALUE #( ( glaccountlineitem         = item1
                                   withholdingtaxcode        = wa_pay-taxcode
                                   withholdingtaxtype        = wa_pay-taxtype
                                   _currencyamount           = VALUE #( ( currencyrole  = '00'
                                                                          taxamount     = wa_pay-tdsamount
                                                                          taxbaseamount = wa_pay-tdsbase
                                                                          currency      = wa_pay-currency   ) ) ) ).

      APPEND LINES OF with_hold_item  TO wa_je_deep-%param-_withholdingtaxitems.

    ENDIF.



  ENDMETHOD.
ENDCLASS.
