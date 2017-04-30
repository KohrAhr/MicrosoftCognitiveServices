/// <summary>
///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uFaceApi;

interface

uses
  { TStream }
  System.Classes,
  { THTTPClient }
  System.Net.HttpClient,
  { TNetHeaders }
  System.Net.URLClient,
  { TContentType }
  uFaceApi.Content.Types,
  { TFaceApiBase }
  uFaceApi.Base,
  { IFaceApi }
  uIFaceApi,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  /// <summary>
  ///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  TFaceApi = class(TFaceApiBase, IFaceApi)
  private
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DetectBase">interface DetectBase</see>
    /// </summary>
    function DetectBase(ARequestType: TContentType; const AData: String; AStreamData: TBytesStream; const ADetectOptions: TDetectOptions): String;
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DetectURL">interface DetectURL</see>
    /// </summary>
    function DetectURL(const AURL: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DetectFile">interface DetectFile</see>
    /// </summary>
    function DetectFile(const AFileName: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DetectStream">interface DetectStream</see>
    /// </summary>
    function DetectStream(AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    function ListPersonGroups(const AStart: String = ''; const ATop: Integer = 1000): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    function ListPersonsInPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.CreatePerson">interface CreatePerson</see>
    /// </summary>
    function CreatePerson(const AGroupID: String; const APersonName: String; const APersonUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.GetPersonGroupTrainingStatus">interface GetPersonGroupTrainingStatus</see>
    /// </summary>
    function GetPersonGroupTrainingStatus(const AGroupID: String): String;
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.TrainPersonGroup">interface TrainPersonGroup</see>
    /// </summary>
    function TrainPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.CreatePersonGroup">interface CreatePersonGroup</see>
    /// </summary>
    function CreatePersonGroup(const AGroupID: String; const AGroupUserData: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DeletePersonGroup">interface DeletePersonGroup</see>
    /// </summary>
    function DeletePersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Verify">interface Verify (overload)</see>
    /// </summary>
    function Verify(const AFaceTempID1, AFaceTempID2: String): String; overload;
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Verify">interface Verify (overload)</see>
    /// </summary>
    function Verify(const AFaceTempID, APersonID, AGroupID: String): String; overload;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Identify">interface Identify</see>
    /// </summary>
    function Identify(AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer = 1; const AConfidenceThreshold: Double = 0.5): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.SetAccessKey">interface SetAccessKey</see>
    /// </summary>
    procedure SetAccessKey(const AAccess: TAccess);
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper,
  { InetHelper }
  uFunctions.InetHelper;

function TFaceApi.CreatePerson(const AGroupID, APersonName, APersonUserData: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format(
    '%s/persongroups/%s/persons',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "name":"%s", "userData":"%s" }',
          [APersonName, APersonUserData]
        )
      )
    );

    Result := InetHelper.PostRequest(GetAccessKey, LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApi.DetectBase(ARequestType: TContentType; const AData: String; AStreamData: TBytesStream; const ADetectOptions: TDetectOptions): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  if ARequestType = rtFile then
    if not FileExists(AData) then
      Exit;

  LURL := Format(
    '%s/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
    [
      ServerBaseUrl,
      BoolToStr(ADetectOptions.FaceId, True).ToLower,
      BoolToStr(ADetectOptions.FaceLandmarks, True).ToLower,
      ADetectOptions.FaceAttributesToString
    ]
  );

  LRequestContent := nil;
  try
    if ARequestType = rtStream then
      LRequestContent := TBytesStream.Create(AStreamData.Bytes)
    else
    if ARequestType = rtUrl then
      LRequestContent := TBytesStream.Create(
        StringHelper.StringToBytesArray(
          Format('{ "url":"%s" }', [AData])
        )
      );

    if ARequestType = rtFile then
      Result := InetHelper.PostRequest(GetAccessKey, LURL, AData, CONST_CONTENT_TYPE[ARequestType])
    else
      Result := InetHelper.PostRequest(GetAccessKey, LURL, LRequestContent, CONST_CONTENT_TYPE[ARequestType]);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApi.DetectFile(const AFileName: String; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtFile, AFileName, nil, ADetectOptions);
end;

function TFaceApi.DetectStream(AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtStream, '', AStream, ADetectOptions);
end;

function TFaceApi.DetectURL(const AURL: String; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtUrl, AURL, nil, ADetectOptions);
end;

function TFaceApi.ListPersonGroups(const AStart: String; const ATop: Integer): String;
var
  LURL: String;
begin
  LURL := Format(
    '%s/persongroups?start=%s&top=%s',
    [
      ServerBaseUrl,
      AStart,
      ATop.ToString
    ]
  );

  Result := InetHelper.GetRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApi.ListPersonsInPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  { Looks like Persons controller method }
  LURL := Format(
    '%s/persongroups/%s/persons',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.GetRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
end;

procedure TFaceApi.SetAccessKey(const AAccess: TAccess);
begin
  Access := AAccess;
end;

function TFaceApi.GetPersonGroupTrainingStatus(const AGroupID: String): String;
var
  LURL: String;
begin
  { Looks like Training controller method }
  LURL := Format(
    '%s/persongroups/%s/training',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.GetRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApi.Identify(AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer; const AConfidenceThreshold: Double): String;
begin
  Result := '';
end;

function TFaceApi.TrainPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  { Looks like Train controller method }
  LURL := Format(
    '%s/persongroups/%s/train',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.PostRequest(GetAccessKey, LURL, nil, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApi.Verify(const AFaceTempID1, AFaceTempID2: String): String;
begin
  Result := '';
end;

function TFaceApi.Verify(const AFaceTempID, APersonID, AGroupID: String): String;
begin
  Result := '';
end;

function TFaceApi.CreatePersonGroup(const AGroupID: String; const AGroupUserData: String): String;
var
  LURL: String;
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
  LRequestContent: TBytesStream;
begin
  { Looks like Default method for controller }
  LURL := Format(
    '%s/persongroups/%s',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  LRequestContent := nil;
  LHTTPClient := InetHelper.PrepareHTTPClient(GetAccessKey, LHeaders, CONST_CONTENT_TYPE_JSON);
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "name":"%s", "userData":"%s" }',
          [AGroupID, AGroupUserData]
        )
      )
    );

    LStream := LHTTPClient.Put(LURL, LRequestContent, nil, LHeaders).ContentStream;

    Result := InetHelper.ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LRequestContent.Free;
    LHTTPClient.Free;
  end;
end;

function TFaceApi.DeletePersonGroup(const AGroupID: String): String;
var
  LURL: String;
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
begin
  { Looks like Default method for controller }
  LURL := Format(
    '%s/persongroups/%s',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  LHTTPClient := InetHelper.PrepareHTTPClient(GetAccessKey, LHeaders, CONST_CONTENT_TYPE_JSON);
  try
    LStream := LHTTPClient.Delete(LURL, nil, LHeaders).ContentStream;

    Result := InetHelper.ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LHTTPClient.Free;
  end;
end;

end.
