unit uFaceApi.Base;

interface

uses
  { TInterfacedPersistent }
  System.Classes,
  { TNetHeader }
  System.Net.URLClient,
  { TAccess }
  uFaceApi.ServersAccess.Types,
  uIFaceApi.Base;

type
  TFaceApiBase = class(TInterfacedObject, IFaceApiBase)
  private
    const
      CONST_FACEAPI_ACCESS_KEY_NAME = 'Ocp-Apim-Subscription-Key';

    var
      FAccess: TAccessServer;
  protected
    function GetAccessKey: TNetHeader;
    function ServerBaseUrl: String;
  private
    property Access: TAccessServer write FAccess;
  public
    procedure SetAccessKey(const AAccess: TAccessServer);


    function GetRequest(const AURL: String; const AContentType: String): String;

    function DeleteRequest(const AURL: String; const AContentType: String): String;

    function PostRequest(const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String; overload;
    function PostRequest(const AURL: String; const AData: string; const AContentType: String): String; overload;

    function PatchRequest(const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String;
    function PutRequest(const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String;
  end;

implementation

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { Format }
  System.SysUtils,
  { InetHelper }
  uFunctions.InetHelper;

procedure TFaceApiBase.SetAccessKey(const AAccess: TAccessServer);
begin
  Access := AAccess;
end;

function TFaceApiBase.GetRequest(const AURL, AContentType: String): String;
begin
  Result := InetHelper.GetRequest(GetAccessKey, ServerBaseUrl + AURL, AContentType);
end;

function TFaceApiBase.PatchRequest(const AURL: String; ARequestContent: TBytesStream;
  const AContentType: String): String;
begin
  Result := InetHelper.PatchRequest(GetAccessKey, ServerBaseUrl + AURL, ARequestContent, AContentType);
end;

function TFaceApiBase.PostRequest(const AURL, AData, AContentType: String): String;
begin
  Result := InetHelper.PostRequest(GetAccessKey, ServerBaseUrl + AURL, AData, AContentType);
end;

function TFaceApiBase.PutRequest(const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String;
begin
  Result := InetHelper.PutRequest(GetAccessKey, ServerBaseUrl + AURL, ARequestContent, AContentType);
end;

function TFaceApiBase.PostRequest(const AURL: String; ARequestContent: TBytesStream;
  const AContentType: String): String;
begin
  Result := InetHelper.PostRequest(GetAccessKey, ServerBaseUrl + AURL, ARequestContent, AContentType);
end;

function TFaceApiBase.DeleteRequest(const AURL, AContentType: String): String;
begin
  Result := InetHelper.DeleteRequest(GetAccessKey, ServerBaseUrl + AURL, AContentType);
end;

function TFaceApiBase.ServerBaseUrl: String;
begin
  Result := Format('https://%s/face/v1.0', [CONST_FACE_API_SERVER_URLS[FAccess.AccessServer]]);
end;

function TFaceApiBase.GetAccessKey: TNetHeader;
begin
  Result.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
  Result.Value := FAccess.AccessKey;
end;

end.
