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
  { TContentType }
  uFaceApi.Content.Types,
  { TFaceApiBase }
  uFaceApi.Base,
  { IFaceApiPersonGroup }
  uIFaceApi.PersonGroup;

type
  /// <summary>
  ///   Main Class implementation "Person Group" functionality for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  TFaceApiCorePersonGroup = class(TFaceApiBase, IFaceApiPersonGroup)
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.CreatePersonGroup">interface CreatePersonGroup</see>
    /// </summary>
    function CreatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.DeletePersonGroup">interface DeletePersonGroup</see>
    /// </summary>
    function DeletePersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.Identify">interface GetPersonGroup</see>
    /// </summary>
    function GetPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.GetPersonGroupTrainingStatus">interface GetPersonGroupTrainingStatus</see>
    /// </summary>
    function GetPersonGroupTrainingStatus(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    function ListPersonGroups(const AStart: String = ''; const ATop: Integer = 1000): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.TrainPersonGroup">interface TrainPersonGroup</see>
    /// </summary>
    function TrainPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.Identify">interface UpdatePersonGroup</see>
    /// </summary>
    function UpdatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper;

function TFaceApiCorePersonGroup.ListPersonGroups(const AStart: String; const ATop: Integer): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups?start=%s&top=%s',
    [
      AStart,
      ATop.ToString
    ]
  );

  Result := GetRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCorePersonGroup.GetPersonGroupTrainingStatus(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s/training',
    [
      AGroupID.ToLower
    ]
  );

  Result := GetRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCorePersonGroup.TrainPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s/train',
    [
      AGroupID.ToLower
    ]
  );

  Result := PostRequest(LURL, nil, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCorePersonGroup.CreatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format(
    '/persongroups/%s',
    [
      AGroupID.ToLower
    ]
  );

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "name":"%s", "userData":"%s" }',
          [AGroupName.ToLower, AGroupUserData]
        )
      )
    );

    Result := PutRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCorePersonGroup.DeletePersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s',
    [
      AGroupID.ToLower
    ]
  );

  Result := DeleteRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiCorePersonGroup.UpdatePersonGroup(const AGroupID, AGroupName, AGroupUserData: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format(
    '/persongroups/%s',
    [
      AGroupID.ToLower
    ]
  );

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "name":"%s", "userData":"%s" }',
          [AGroupName.ToLower, AGroupUserData]
        )
      )
    );

    Result :=  PatchRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCorePersonGroup.GetPersonGroup(const AGroupID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s',
    [
      AGroupID.ToLower
    ]
  );

  Result := GetRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

end.