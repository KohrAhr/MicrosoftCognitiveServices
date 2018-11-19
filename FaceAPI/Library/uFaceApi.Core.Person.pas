/// <summary>
///   Main Class implementation for "Person" functionality for Face API
///   Microsoft Cognitive Services 1.0
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
    function CreatePerson(const AGroupID: String; const APersonName: String;
      const APersonUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.DeletePerson">
    ///   interface DeletePerson</see>
    /// </summary>
    function DeletePerson(const AGroupID, APersonID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Person|IFaceApiPerson.ListPersonsInPersonGroup">
    ///   interface ListPersonsInPersonGroup</see>
    /// </summary>
    function ListPersonsInPersonGroup(const AGroupID: String): String;
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper;

function TFaceApiPerson.CreatePerson(const AGroupID, APersonName,
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

function TFaceApiPerson.DeletePerson(const AGroupID,
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

function TFaceApiPerson.ListPersonsInPersonGroup(const AGroupID: String): String;
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

end.
