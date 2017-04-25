unit uFaceApi;

interface

uses
  System.Classes,
  { TFaceApiServer }
  uFaceApi.Servers.Types;

type
  TFaceApiBase = class
  private
    FAccessKey: String;
    FAccessServer: TFaceApiServer;
  public
    property AccessKey: String read FAccessKey write FAccessKey;
    property AccessServer: TFaceApiServer read FAccessServer write FAccessServer;
  end;

  TFaceApi = class(TFaceApiBase)
  public
    function DetectURL(AURL: String; AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = ''): String; overload;
    function DetectFile(AFileName: String; AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = ''): String; overload;

    constructor Create(const AAccessKey: String; const AAccessServer: TFaceApiServer = fasGeneral);
  end;

implementation

uses
  { THTTPClient }
  System.Net.HttpClient,
  { TNetHeaders }
  System.Net.URLClient,
  { ContentType }
  System.NetConsts,
  { Format }
  System.SysUtils, uFunctions.StringHelper;

{ TFaceApi }

constructor TFaceApi.Create(const AAccessKey: String; const AAccessServer: TFaceApiServer = fasGeneral);
begin
  inherited Create;

  AccessKey := AAccessKey;

  FAccessServer := AAccessServer;
end;


function TFaceApi.DetectFile(AFileName: String; AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = ''): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
begin
  if not FileExists(AFileName) then
    Exit;

  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := 'Ocp-Apim-Subscription-Key';
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LHTTPClient.ContentType := 'application/octet-stream';
    LURL := Format(
      'https://%s/face/v1.0/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        BoolToStr(AReturnFaceId, True).ToLower,
        BoolToStr(AReturnFaceLandmarks, True).ToLower,
        AReturnFaceAttributes.ToLower
      ]
    );
    LStream := LHTTPClient.Post(LURL, AFileName, nil, LHeaders).ContentStream;

    LResponse := TMemoryStream.Create;
    try
      LResponse.CopyFrom(LStream, LStream.Size);

      Result := StringHelper.MemoryStreamToString(LResponse);
    finally
      LResponse.Free;
    end;
  finally
    LHTTPClient.Free;
  end;
end;

function TFaceApi.DetectURL(AURL: String; AReturnFaceId, AReturnFaceLandmarks: Boolean; AReturnFaceAttributes: String): String;
//var
//  LNameValuePair: TNameValuePair;
//  LHTTPClient: THTTPClient;
//	LStream: TStream;
//  LURL: String;
//  LHeaders: TNetHeaders;
//	LResponse: TMemoryStream;
begin
//  LHTTPClient := THTTPClient.Create;
//  try
//    SetLength(LHeaders, 1);
//
//    LNameValuePair.Name := 'Ocp-Apim-Subscription-Key';
//    LNameValuePair.Value := AccessKey;
//    LHeaders[0] := LNameValuePair;
//
//    LHTTPClient.ContentType := 'application/json';
//    LURL := Format(
//      'https://%s/face/v1.0/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
//      [
//        CONST_FACE_API_SERVER_URL[AccessServer],
//        BoolToStr(AReturnFaceId, True).ToLower,
//        BoolToStr(AReturnFaceLandmarks, True).ToLower,
//        AReturnFaceAttributes.ToLower
//      ]
//    );
//    LStream := LHTTPClient.Post(LURL, AURL, nil, LHeaders).ContentStream;
//
//    LResponse := TMemoryStream.Create;
//    try
//      LResponse.CopyFrom(LStream, LStream.Size);
//
//      Result := StringHelper.MemoryStreamToString(LResponse);
//    finally
//      LResponse.Free;
//    end;
//  finally
//    LHTTPClient.Free;
//  end;
end;

end.
