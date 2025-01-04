*&---------------------------------------------------------------------*
*& Report ZGSP_API_CONNECT
*&---------------------------------------------------------------------*
*& GST E-Invoicing GSP API Connectivity Solution (SAP ABAP Code)
*& Please review first then adjust the code
*& 
*& Details:
*& https://github.com/sujianalytics/gst-e-invoicing-sap
*& 
*& Note: You need to use dynamic data instead of static invoice number 
*& in the following code. This is only connectivity solution, this code 
*& does not contains any busines data mapping.
*&---------------------------------------------------------------------*

REPORT zgsp_api_connect
       LINE-SIZE 400 NO STANDARD PAGE HEADING.

DATA: lt_file_table TYPE rsanm_file_table.

*&---------------------------------------------------------------------*
*& Create payload and save into JSON file
*&---------------------------------------------------------------------*
CLEAR: lt_file_table.

" Dynamic invoice number
DATA(lv_invoice_number) = 'INV123'. " Replace with dynamic data

"To have following data in payload
*[{
*  "transaction": {
*    "Version": "1.1",
*    "TranDtls": {
*      "TaxSch": "GST",
*      "SupTyp": "B2B",
*      "RegRev": "Y",
*      "EcmGstin": null,
*      "IgstOnIntra": "N",
*      "DocNumer": "INV123"
*    }
*  }
*}]
*
" You need to enter them line by as shown below:

APPEND |[ { "transaction": { "Version": "1.1", "TranDtls": { "TaxSch": "GST", "SupTyp": "B2B", "
                              RegRev": "Y", "EcmGstin": null, "IgstOnIntra": "N", "DocNumer": "{ 
                              lv_invoice_number }" } } }] | TO lt_file_table.

CALL METHOD cl_rsan_ut_appserv_file_writer=>appserver_file_write
  EXPORTING
    i_filename = |/usr/sap/SID/D01/work/{ lv_invoice_number }_P.json|
    i_overwrite = abap_true
    i_data_tab = lt_file_table
  EXCEPTIONS
    open_failed  = 1
    write_failed = 2
    close_failed = 3
    OTHERS       = 4.

IF sy-subrc IS INITIAL.
  WRITE:/ 'File Write', 'Success'.
ELSE.
  WRITE:/ 'File Write', 'Error'.
ENDIF.

*&---------------------------------------------------------------------*
*& Trigger data send process and wait for result
*&---------------------------------------------------------------------*
WRITE:/ 'Send Data', 'Started'.
CALL 'SYSTEM' ID 'COMMAND' FIELD |bash sujigspcon.sh sujiapi.conf { lv_invoice_number }_P.json|
              ID 'TAB'     FIELD tabl[]. "<--- CLI return found here

IF sy-subrc IS INITIAL.
  WRITE:/ 'Send Data', 'Success'.
  WRITE:/ '====================================================='.
  LOOP AT tabl.
    WRITE:/ tabl.
  ENDLOOP.
ELSE.
  WRITE:/ 'Send Data', 'Error', sy-subrc.
ENDIF.

*&---------------------------------------------------------------------*
*& Read received data
*&---------------------------------------------------------------------*
CLEAR: lt_file_table.

CALL METHOD cl_rsan_ut_appserv_file_reader=>appserver_file_read
  EXPORTING
    i_filename = |/usr/sap/JPD/D01/work/{ lv_invoice_number }_R.json|
  CHANGING
    c_data_tab = lt_file_table
  EXCEPTIONS
    open_failed  = 1
    read_failed  = 2
    close_failed = 3
    OTHERS       = 4.

IF sy-subrc IS INITIAL.
  WRITE:/ 'File Read', 'Success'.
  WRITE:/ '====================================================='.
  LOOP AT lt_file_table INTO DATA(ls_file_table).
    WRITE:/ ls_file_table.
  ENDLOOP.
ELSE.
  WRITE:/ 'File Read', 'Error', 'File not found'.
ENDIF.
