/// <summary>
///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uFaceApi.Core;

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
  TFaceApiCore = class(TFaceApiBase, IFaceApi)
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
    ///   Implements <see cref="uIFaceApi|IFaceApi.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    function ListPersonsInPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.CreatePerson">interface CreatePerson</see>
    /// </summary>
    function CreatePerson(const AGroupID: String; const APersonName: String; const APersonUserData: String = ''): String;

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
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper,
  { InetHelper }
  uFunctions.InetHelper;

function TFaceApiCore.CreatePerson(const AGroupID, APersonName, APersonUserData: String): String;
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

function TFaceApiCore.DetectBase(ARequestType: TContentType; const AData: String; AStreamData: TBytesStream; const ADetectOptions: TDetectOptions): String;
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

function TFaceApiCore.DetectFile(const AFileName: String; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtFile, AFileName, nil, ADetectOptions);
end;

function TFaceApiCore.DetectStream(AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtStream, '', AStream, ADetectOptions);
end;

function TFaceApiCore.DetectURL(const AURL: String; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtUrl, AURL, nil, ADetectOptions);
end;

function TFaceApiCore.ListPersonsInPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '%s/persongroups/%s/persons',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.GetRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCore.Verify(const AFaceTempID1, AFaceTempID2: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format('%s/verify', [ServerBaseUrl]);

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "faceId1":"%s", "faceId2":"%s" }',
          [AFaceTempID1, AFaceTempID2]
        )
      )
    );

    Result := InetHelper.PostRequest(GetAccessKey, LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCore.Verify(const AFaceTempID, APersonID, AGroupID: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format('%s/verify', [ServerBaseUrl]);

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "faceId":"%s", "personGroupId":"%s", "personId":"%s" }',
          [AFaceTempID, AGroupID.ToLower, APersonID]
        )
      )
    );

    Result := InetHelper.PostRequest(GetAccessKey, LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCore.Identify(AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer; const AConfidenceThreshold: Double): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format('%s/identify', [ServerBaseUrl]);

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(Format(
          '{ "personGroupId":"%s", "faceIds":[%s], "maxNumOfCandidatesReturned":%s,"confidenceThreshold":%s }',
          [AGroupID.ToLower, StringHelper.StringListToGuidsString(AFaceIDS, '"', ','), AMaxNumOfCandidatesReturned.ToString, AConfidenceThreshold.ToString]
        )
      )
    );

    Result := InetHelper.PostRequest(GetAccessKey, LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

end.
