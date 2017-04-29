unit uFaceApi.Servers.Types;

interface

type
  TFaceApiServer = (
    fasGeneral,
    fasWestUS,
    fasEastUS2,
    fasWestCentralUS,
    fasWestEurope,
    fasSoutheastAsia
  );

const
  CONST_FACE_API_MAIN_SERVER = 'api.cognitive.microsoft.com';

  CONST_FACE_API_SERVER_URLS: array [TFaceApiServer] of string = (
    CONST_FACE_API_MAIN_SERVER,
    'westus.' + CONST_FACE_API_MAIN_SERVER,
    'eastus2.' + CONST_FACE_API_MAIN_SERVER,
    'westcentralus.' + CONST_FACE_API_MAIN_SERVER,
    'westeurope.' + CONST_FACE_API_MAIN_SERVER,
    'southeastasia.' + CONST_FACE_API_MAIN_SERVER
  );

implementation

end.
