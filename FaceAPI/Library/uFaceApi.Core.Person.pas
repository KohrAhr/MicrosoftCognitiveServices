/// <summary>
///   Main Class implementation for "PersonGroup Person" functionality for Face
///   API Microsoft Cognitive Services 1.0
/// </summary>
unit uFaceApi.Core.Person;

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
  { IFaceApiPerson }
  uIFaceApi.Person;

type
  /// <summary>
  ///   Main Class implementation "Person" functionality for Face API Microsoft
  ///   Cognitive Services 1.0
  /// </summary>
  TFaceApiPerson = class(TFaceApiBase, IFaceApiPerson)
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.CreatePerson">
    ///   interface CreatePerson</see>
    /// </summary>
    function New(const AGroupID: String; const APersonName: String;
      const APersonUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.DeletePerson">
    ///   interface DeletePerson</see>
    /// </summary>
    function Delete(const AGroupID, APersonID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.ListPersonsInPersonGroup">
    ///   interface ListPersonsInPersonGroup</see>
    /// </summary>
    function List(const AGroupID, AStart: String;
      ATop: Integer): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.DeletePersonFace">
    ///   interface DeletePersonFace</see>
    /// </summary>
    function DeletePersonFace(const AGroupID, APersonID: String;
      APersistedFaceID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.AddPersonFaceURL">
    ///   interface AddPersonFaceURL</see>
    /// </summary>
    function AddPersonFaceURL(const AGroupID, APersonID, AURL: String;
      ATargetFace: String; const AUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.AddPersonFaceStream"
    ///   >interface AddPersonFaceStream</see>
    /// </summary>
    function AddPersonFaceStream(const AGroupID, APersonID: String; AStreamData:
      TBytesStream; ATargetFace: String; const AUserData: String = ''): String;
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper;

function TFaceApiPerson.New(const AGroupID, APersonName,
  APersonUserData: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format(
    '/persongroups/%s/persons',
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
          [APersonName, APersonUserData]
        )
      )
    );

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiPerson.Delete(const AGroupID,
  APersonID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s/persons/%s',
    [
      AGroupID.ToLower, APersonID.ToLower
    ]
  );

  Result := DeleteRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiPerson.List(const AGroupID, AStart: String;
  ATop: Integer): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s/persons',
    [
      AGroupID.ToLower
    ]
  );

  Result := GetRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiPerson.DeletePersonFace(const AGroupID, APersonID: String;
  APersistedFaceID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '/persongroups/%s/persons/%s/persistedFaces/%s',
    [
      AGroupID.ToLower, APersonID.ToLower, APersistedFaceID
    ]
  );

  Result := DeleteRequest(LURL, CONST_CONTENT_TYPE_JSON);
end;

function TFaceApiPerson.AddPersonFaceStream(const AGroupID, APersonID: String;
  AStreamData: TBytesStream; ATargetFace: String; const AUserData: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format(
    '/persongroups/%s/persons/%s/persistedFaces?userData=%s&targetFace=%s',
    [
      AGroupID.ToLower, APersonID, AUserData, ATargetFace
    ]
  );

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(AStreamData.Bytes);

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiPerson.AddPersonFaceURL(const AGroupID, APersonID, AURL: String;
  ATargetFace: String; const AUserData: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := Format(
    '/persongroups/%s/persons/%s/persistedFaces?userData=%s&targetFace=%s',
    [
      AGroupID.ToLower, APersonID, AUserData, ATargetFace
    ]
  );

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(
        Format(
          '{ "url":"%s" }',
          [AURL]
        )
      )
    );

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

end.
