unit uFaceApi.Content.Types;

interface

type
  TContentType = (rtFile, rtUrl, rtStream);

const
  CONST_CONTENT_TYPE_JSON = 'application/json';
  CONST_CONTENT_TYPE_BIN = 'application/octet-stream';

  CONST_CONTENT_TYPE: array [TContentType] of String = (
    CONST_CONTENT_TYPE_BIN,
    CONST_CONTENT_TYPE_JSON,
    CONST_CONTENT_TYPE_BIN
  );

implementation

end.
