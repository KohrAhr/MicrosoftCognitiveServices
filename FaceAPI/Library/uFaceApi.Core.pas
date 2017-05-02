/// <summary>
///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uFaceApi.Core;

interface

uses
  { TBytesStream }
  System.Classes,
  { TContentType }
  uFaceApi.Content.Types,
  { TFaceApiBase }
  uFaceApi.Base,
  { IFaceApi }
  uIFaceApi;

type
  /// <summary>
  ///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  TFaceApiCore = class(TFaceApiBase, IFaceApi)
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    function ListPersonsInPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.CreatePerson">interface CreatePerson</see>
    /// </summary>
    function CreatePerson(const AGroupID: String; const APersonName: String; const APersonUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.DeletePerson">interface DeletePerson</see>
    /// </summary>
    function DeletePerson(const AGroupID, APersonID: String): String;
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

function TFaceApiCore.DeletePerson(const AGroupID, APersonID: String): String;
var
  LURL: String;
begin
  LURL := Format(
    '%s/persongroups/%s/persons/%s',
    [
      ServerBaseUrl,
      AGroupID.ToLower, APersonID.ToLower
    ]
  );

  Result := InetHelper.DeleteRequest(GetAccessKey, LURL, CONST_CONTENT_TYPE_JSON);
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


end.
