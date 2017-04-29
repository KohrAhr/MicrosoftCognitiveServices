unit uFunctions.InetHelper;

interface

uses
  { THTTPClient }
  System.Net.HttpClient,
  { TNetHeaders }
  System.Net.URLClient,
  { TBytesStream }
  System.Classes;

type
  InetHelper = class
  private
    class function PostRequestBase(AAccessKey: TNetHeader; const AURL, AData: String; ARequestContent: TBytesStream; const AContentType: String): String;
  public
    class function GetRequest(AAccessKey: TNetHeader; const AURL: String; const AContentType: String): String;

    class function PostRequest(AAccessKey: TNetHeader; const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String; overload;
    class function PostRequest(AAccessKey: TNetHeader; const AURL: String; const AData: string; const AContentType: String): String; overload;

    class function PrepareHttpClient(AAccessKey: TNetHeader; var AHeaders: TNetHeaders; const AContentType: String): THTTPClient;
    class function ProceedHttpClientData(AClient: THTTPClient; AData: TStream): String;
  end;

implementation

uses
  { StringHelper }
  uFunctions.StringHelper,
  { Exception }
  System.SysUtils,
  { SetContentType }
  System.NetConsts;

class function InetHelper.GetRequest(AAccessKey: TNetHeader; const AURL: String; const AContentType: String): String;
var
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
begin
  LHTTPClient := PrepareHTTPClient(AAccessKey, LHeaders, AContentType);
  try
    LStream := LHTTPClient.Get(AURL, nil, LHeaders).ContentStream;

    Result := ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LHTTPClient.Free;
  end;
end;

class function InetHelper.PostRequestBase(AAccessKey: TNetHeader; const AURL, AData: String; ARequestContent: TBytesStream; const AContentType: String): String;
var
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LHeaders: TNetHeaders;
begin
  LHTTPClient := PrepareHTTPClient(AAccessKey, LHeaders, AContentType);
  try
    if AData <> '' then
      LStream := LHTTPClient.Post(AURL, AData, nil, LHeaders).ContentStream
    else
      LStream := LHTTPClient.Post(AURL, ARequestContent, nil, LHeaders).ContentStream;

    Result := ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LHTTPClient.Free;
  end;
end;

class function InetHelper.PostRequest(AAccessKey: TNetHeader; const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String;
begin
  Result := PostRequestBase(AAccessKey, AURL, '', ARequestContent, AContentType);
end;

class function InetHelper.PostRequest(AAccessKey: TNetHeader; const AURL, AData, AContentType: String): String;
begin
  Result := PostRequestBase(AAccessKey, AURL, AData, nil, AContentType);
end;

class function InetHelper.PrepareHttpClient(AAccessKey: TNetHeader; var AHeaders: TNetHeaders; const AContentType: String): THTTPClient;
var
  LNameValuePair: TNameValuePair;
begin
  Result := THTTPClient.Create;
  try
    if (AAccessKey.Name = '') or (AAccessKey.Value = '') then
      raise Exception.Create('License Key is required');

    SetLength(AHeaders, 1);

    LNameValuePair.Name := AAccessKey.Name;
    LNameValuePair.Value := AAccessKey.Value;
    AHeaders[0] := LNameValuePair;

    Result.ContentType := AContentType;
  except
    Result.Free;
    raise;
  end;
end;

class function InetHelper.ProceedHttpClientData(AClient: THTTPClient; AData: TStream): String;
var
	LResponse: TMemoryStream;
begin
  LResponse := TMemoryStream.Create;
  try
    AData.Position := 0;
    LResponse.CopyFrom(AData, AData.Size);

    Result := StringHelper.MemoryStreamToString(LResponse);
  finally
    LResponse.Free;
  end;
end;

end.
