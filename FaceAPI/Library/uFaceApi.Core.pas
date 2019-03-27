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
  { IFaceApiCore }
  uIFaceApi.Core;

type
  /// <summary>
  ///   Main Class implementation for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  TFaceApiCore = class(TFaceApiBase, IFaceApiCore)
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Core|IFaceApiCore.AddPersonFaceURL">
    ///   interface AddPersonFaceURL</see>
    /// </summary>
    function AddPersonFaceURL(const AGroupID, APersonID, AURL: String;
      ATargetFace: String; const AUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Core|IFaceApiCore.AddPersonFaceStream"
    ///   >interface AddPersonFaceStream</see>
    /// </summary>
    function AddPersonFaceStream(const AGroupID, APersonID: String; AStreamData:
      TBytesStream; ATargetFace: String; const AUserData: String = ''): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Core|IFaceApiCore.DeletePersonFace">
    ///   interface DeletePersonFace</see>
    /// </summary>
    function DeletePersonFace(const AGroupID, APersonID: String;
      APersistedFaceID: String): String;
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper;

function TFaceApiCore.AddPersonFaceStream(const AGroupID, APersonID: String;
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

function TFaceApiCore.AddPersonFaceURL(const AGroupID, APersonID, AURL: String;
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

function TFaceApiCore.DeletePersonFace(const AGroupID, APersonID: String;
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

end.
