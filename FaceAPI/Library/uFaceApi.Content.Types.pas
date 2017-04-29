unit uFaceApi.Content.Types;

interface

type
  TContentType = (rtFile, rtUrl, rtStream);

const
  CONST_JSON = 'application/json';

  CONST_CONTENT_TYPE: array [TContentType] of String = (
    'application/octet-stream',
    'application/json',
    'application/octet-stream'
  );

implementation

end.
