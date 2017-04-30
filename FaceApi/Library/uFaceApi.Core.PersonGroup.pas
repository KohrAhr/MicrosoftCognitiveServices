/// <summary>
///   Main Class implementation for "Person Group" functionality for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uFaceApi.Core.PersonGroup;

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
  { IFaceApiPersonGroup }
  uIFaceApi.PersonGroup,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  /// <summary>
  ///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  TFaceApiCorePersonGroup = class(TFaceApiBase, IFaceApiPersonGroup)
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.CreatePersonGroup">interface CreatePersonGroup</see>
    /// </summary>
    function CreatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DeletePersonGroup">interface DeletePersonGroup</see>
    /// </summary>
    function DeletePersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Identify">interface GetPersonGroup</see>
    /// </summary>
    function GetPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.GetPersonGroupTrainingStatus">interface GetPersonGroupTrainingStatus</see>
    /// </summary>
    function GetPersonGroupTrainingStatus(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    function ListPersonGroups(const AStart: String = ''; const ATop: Integer = 1000): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.TrainPersonGroup">interface TrainPersonGroup</see>
    /// </summary>
    function TrainPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Identify">interface UpdatePersonGroup</see>
    /// </summary>
    function UpdatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper,
  { InetHelper }
  uFunctions.InetHelper;

function TFaceApiCorePersonGroup.ListPersonGroups(const AStart: String; const ATop: Integer): String;
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

function TFaceApiCorePersonGroup.GetPersonGroupTrainingStatus(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '%s/persongroups/%s/training',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.GetRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCorePersonGroup.TrainPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '%s/persongroups/%s/train',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.PostRequest(GetAccessKey, LURL, nil, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCorePersonGroup.CreatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;
var
  LURL: String;
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
  LRequestContent: TBytesStream;
begin
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
          [AGroupName.ToLower, AGroupUserData]
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

function TFaceApiCorePersonGroup.DeletePersonGroup(const AGroupID: String): String;
var
  LURL: String;
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
begin
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

function TFaceApiCorePersonGroup.UpdatePersonGroup(const AGroupID, AGroupName, AGroupUserData: String): String;
var
  LURL: String;
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
  LRequestContent: TBytesStream;
begin
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
          [AGroupName.ToLower, AGroupUserData]
        )
      )
    );

    LStream := LHTTPClient.Patch(LURL, LRequestContent, nil, LHeaders).ContentStream;

    Result := InetHelper.ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LRequestContent.Free;
    LHTTPClient.Free;
  end;
end;

function TFaceApiCorePersonGroup.GetPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '%s/persongroups/%s',
    [
      ServerBaseUrl,
      AGroupID.ToLower
    ]
  );

  Result := InetHelper.GetRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
end;

end.
