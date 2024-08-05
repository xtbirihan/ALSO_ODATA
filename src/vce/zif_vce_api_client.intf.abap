interface ZIF_VCE_API_CLIENT
  public .


  "! <p class="shorttext synchronized" lang="en">Build the data for the request</p>
  methods BUILD_REQUEST_DATA
    importing
      !IV_INPUT type ANY
    exporting
      !ES_DATA type ANY .
  "! <p class="shorttext synchronized" lang="en">Get the response of the request</p>
  methods GET_RESPONSE
    importing
      !IV_JSON type /UI2/CL_JSON=>JSON optional
    exporting
      !EV_STATUS type I
      !ES_DATA type ANY .
  "! <p class="shorttext synchronized" lang="en">Process the response of the request</p>
  methods PROCESS_RESPONSE
    importing
      !IV_JSON type /UI2/CL_JSON=>JSON optional .
  "! <p class="shorttext synchronized" lang="en">Send get request</p>
  methods SEND_GET_REQUEST .
  "! <p class="shorttext synchronized" lang="en">Send post request</p>
  methods SEND_POST_REQUEST
    importing
      !IS_DATA type ANY
    exporting
      !EV_JSON type /UI2/CL_JSON=>JSON .
endinterface.
