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
  CONST_FACE_API_SERVER_URL: array [TFaceApiServer] of string = (
    'api.cognitive.microsoft.com',
    'westus.api.cognitive.microsoft.com',
    'eastus2.api.cognitive.microsoft.com',
    'westcentralus.api.cognitive.microsoft.com',
    'westeurope.api.cognitive.microsoft.com',
    'southeastasia.api.cognitive.microsoft.com'
  );

implementation

end.
