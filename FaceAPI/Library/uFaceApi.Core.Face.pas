/// <summary>
///   Main Class implementation for "Face" functionality for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uFaceApi.Core.Face;

interface

uses
  { TBytesStream }
  System.Classes,
  { TContentType }
  uFaceApi.Content.Types,
  { TFaceApiBase }
  uFaceApi.Base,
  { IFaceApiFace }
  uIFaceApi.Face,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions;

type
  /// <summary>
  ///   Main Class implementation for "Face" functionality for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  TFaceApiCoreFace = class(TFaceApiBase, IFaceApiFace)
  private
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.DetectBase">interface DetectBase</see>
    /// </summary>
    function DetectBase(ARequestType: TContentType; const AData: String; AStreamData: TBytesStream; const ADetectOptions: TDetectOptions): String;
  public
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.DetectURL">interface DetectURL</see>
    /// </summary>
    function DetectURL(const AURL: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.DetectFile">interface DetectFile</see>
    /// </summary>
    function DetectFile(const AFileName: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.DetectStream">interface DetectStream</see>
    /// </summary>
    function DetectStream(AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar</see>
    /// </summary>
    function FindSimilar(const AFaceID: String; const AListID: String; const AMaxNumOfCandidatesReturned: Integer = 20; AFindMode: String = 'matchPerson'): String; overload;
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar</see>
    /// </summary>
    function FindSimilar(const AFaceID: String; AFaceIDS: TStringList; const AMaxNumOfCandidatesReturned: Integer = 20; AFindMode: String = 'matchPerson'): String; overload;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.Group">interface Group</see>
    /// </summary>
    function Group(AFaceIDS: TStringList): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.Identify">interface Identify</see>
    /// </summary>
    function Identify(AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer = 1; const AConfidenceThreshold: Double = 0.5): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.Verify">interface Verify (overload)</see>
    /// </summary>
    function Verify(const AFaceTempID1, AFaceTempID2: String): String; overload;
    /// <summary>
    ///   Implements <see cref="uIFaceApi.Face|IFaceApiFace.Verify">interface Verify (overload)</see>
    /// </summary>
    function Verify(const AFaceTempID, APersonID, AGroupID: String): String; overload;
  end;

implementation

uses
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper;

function TFaceApiCoreFace.DetectBase(ARequestType: TContentType; const AData: String; AStreamData: TBytesStream; const ADetectOptions: TDetectOptions): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  if ARequestType = rtFile then
    if not FileExists(AData) then
      Exit;

  LURL := Format(
    '/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
    [
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
      Result := PostRequest(LURL, AData, CONST_CONTENT_TYPE[ARequestType])
    else
      Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE[ARequestType]);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCoreFace.DetectFile(const AFileName: String; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtFile, AFileName, nil, ADetectOptions);
end;

function TFaceApiCoreFace.DetectStream(AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtStream, '', AStream, ADetectOptions);
end;

function TFaceApiCoreFace.DetectURL(const AURL: String; const ADetectOptions: TDetectOptions): String;
begin
  Result := DetectBase(rtUrl, AURL, nil, ADetectOptions);
end;

function TFaceApiCoreFace.FindSimilar(const AFaceID, AListID: String; const AMaxNumOfCandidatesReturned: Integer; AFindMode: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := '/findsimilars';

  LRequestContent := nil;
  try

    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(Format(
          '{ "faceId":[%s], "faceListId":"%s", "maxNumOfCandidatesReturned":%s, "mode":"%s" }',
          [AFaceID, AListID, AMaxNumOfCandidatesReturned.ToString, AFindMode]
        )
      )
    );

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCoreFace.FindSimilar(const AFaceID: String; AFaceIDS: TStringList; const AMaxNumOfCandidatesReturned: Integer; AFindMode: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := '/findsimilars';

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(Format(
          '{ "faceId":[%s], "faceIds":["%s"], "maxNumOfCandidatesReturned":%s, "mode":"%s" }',
          [AFaceID, StringHelper.StringListToGuidsString(AFaceIDS, '"', ','), AMaxNumOfCandidatesReturned.ToString, AFindMode]
        )
      )
    );

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCoreFace.Group(AFaceIDS: TStringList): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := '/group';

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(Format(
          '{ "faceIds":[%s] }',
          [StringHelper.StringListToGuidsString(AFaceIDS, '"', ',')]
        )
      )
    );

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCoreFace.Identify(AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer; const AConfidenceThreshold: Double): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := '/identify';

  LRequestContent := nil;
  try
    LRequestContent := TBytesStream.Create(
      StringHelper.StringToBytesArray(Format(
          '{ "personGroupId":"%s", "faceIds":[%s], "maxNumOfCandidatesReturned":%s,"confidenceThreshold":%s }',
          [AGroupID.ToLower, StringHelper.StringListToGuidsString(AFaceIDS, '"', ','), AMaxNumOfCandidatesReturned.ToString, AConfidenceThreshold.ToString]
        )
      )
    );

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCoreFace.Verify(const AFaceTempID1, AFaceTempID2: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := '/verify';

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

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

function TFaceApiCoreFace.Verify(const AFaceTempID, APersonID, AGroupID: String): String;
var
  LURL: String;
  LRequestContent: TBytesStream;
begin
  LURL := '/verify';

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

    Result := PostRequest(LURL, LRequestContent, CONST_CONTENT_TYPE_JSON);
  finally
    LRequestContent.Free;
  end;
end;

end.
