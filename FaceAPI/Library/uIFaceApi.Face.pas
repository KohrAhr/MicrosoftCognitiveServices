/// <summary>
///   Unit contain the Main Interface declaration for "Face" functionality for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uIFaceApi.Face;

interface

uses
  { TStringStream }
  System.Classes,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TContentType }
  uFaceApi.Content.Types,
  { IFaceApiBase }
  uIFaceApi.Base;

type
  /// <summary>
  ///   Main Interface for "Face" functionality for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  IFaceApiFace = interface(IFaceApiBase)
    ['{B4DAE3AA-C4F8-4E4A-9281-85FAC6533EA7}']

    /// <summary>
    ///   Base function for Detect human faces in an image and returns face locations, and optionally with faceIds, landmarks, and attributes.
    ///   Optional parameters for returning faceId, landmarks, and attributes. Attributes include age, gender, smile intensity, facial hair, head pose and glasses. faceId is for other APIs use including Face - Identify, Face - Verify, and Face - Find Similar. The faceId will expire 24 hours after detection call.
    ///   JPEG, PNG, GIF(the first frame), and BMP are supported. The image file size should be larger than or equal to 1KB but no larger than 4MB.
    ///   The detectable face size is between 36x36 to 4096x4096 pixels. The faces out of this range will not be detected.
    ///   A maximum of 64 faces could be returned for an image. The returned faces are ranked by face rectangle size in descending order.
    ///   Some faces may not be detected for technical challenges, e.g. very large face angles (head-pose) or large occlusion. Frontal and near-frontal faces have the best results.
    ///   Attributes (age, gender, headPose, smile, facialHair, glasses and emotion) are still experimental and may not be very accurate. HeadPose's pitch value is a reserved field and will always return 0.
    ///   https://westus.dev.cognitive.microsoft.com/docs/services/563879b61984550e40cbbe8d/operations/563879b61984550f30395236
    /// </summary>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an array of face entries ranked by face rectangle size in descending order. An empty response indicates no faces detected.
    ///   A face entry may contain the following values depending on input parameters:
    ///   Fields	        Type	  Description
    ///   faceId	        String	Unique faceId of the detected face, created by detection API and it will expire 24 hours after detection call. To return this, it requires "returnFaceId" parameter to be true.
    ///   faceRectangle	  Object	A rectangle area for the face location on image.
    ///   faceLandmarks	  Object	An array of 27-point face landmarks pointing to the important positions of face components. To return this, it requires "returnFaceLandmarks" parameter to be true.
    ///   faceAttributes	Object 	Face Attributes:
    ///     age: an age number in years.
    ///     gender: male or female.
    ///     smile: smile intensity, a number between [0,1]
    ///     facialHair: consists of lengths of three facial hair areas: moustache, beard and sideburns.
    ///     headPose: 3-D roll/yew/pitch angles for face direction. Pitch value is a reserved field and will always return 0.
    ///     glasses: glasses type. Possible values are 'noGlasses', 'readingGlasses', 'sunglasses', 'swimmingGoggles'.
    ///     emotion: emotions intensity expressed by the face, incluing anger, contempt, disgust, fear, happiness, neutral, sadness and surprise.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 408, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code  Error Message Description
    /// </returns>
    function DetectBase(ARequestType: TContentType; const AData: String; AStreamData: TBytesStream; const ADetectOptions: TDetectOptions): String;

    /// <summary>
    ///   Detect human faces in an image (internet url) and returns face locations, and optionally with faceIds, landmarks, and attributes.
    ///   Wrapper for base <see cref="uIFaceApi|IFaceApi.DetectBase">Detect function</see>
    /// </summary>
		/// <param name="AURL">
    ///   Internet URL of image. You cannot use intranet URL because it will be not accessable by Microsoft Cognitive Services servers.
    /// </param>
		/// <param name="ADetectOptions">
    ///   Face detect options and attributes
    /// </param>
    /// <returns>
    ///   <see cref="uIFaceApi|IFaceApi.DetectBase">See base Detect function</see>
    /// </returns>
    function DetectURL(const AURL: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Detect human faces in an image (local file name) and returns face locations, and optionally with faceIds, landmarks, and attributes.
    ///   Wrapper for base Detect function
    /// </summary>
		/// <param name="AFileName">
    ///   Local file name with image
    /// </param>
		/// <param name="ADetectOptions">
    ///   Face detect options and attributes
    /// </param>
    /// <returns>
    ///   <see cref="uIFaceApi|IFaceApi.DetectBase">See base Detect function</see>
    /// </returns>
    function DetectFile(const AFileName: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Detect human faces in an image (stream) and returns face locations, and optionally with faceIds, landmarks, and attributes.
    ///   Wrapper for base Detect function
    /// </summary>
		/// <param name="AStream">
    ///   Stream with image
    /// </param>
		/// <param name="ADetectOptions">
    ///   Face detect options and attributes
    /// </param>
    /// <returns>
    ///   <see cref="uIFaceApi|IFaceApi.DetectBase">See base Detect function</see>
    /// </returns>
    function DetectStream(AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;

    /// <summary>
    ///   Given query face's faceId, to search the similar-looking faces from a faceId array or a faceListId.
    ///   faceId array contains the faces created by Face - Detect, which will expire 24 hours after creation.
    ///   While "faceListId" is created by Face List - Create a Face List containing persistedFaceIds that will not expire.
    ///   Depending on the input the returned similar faces list contains faceIds or persistedFaceIds ranked by similarity.
    /// </summary>
    /// <param name="AFaceID">
    ///   faceId of the query face. User needs to call Face - Detect first to get a valid faceId.
    ///   Note that this faceId is not persisted and will expire 24 hours after the detection call.
    /// </param>
    /// <param name="AListID">
    ///   An existing user-specified unique candidate face list, created in Face List - Create a Face List.
    ///   Face list contains a set of persistedFaceIds which are persisted and will never expire.
    ///   Parameter faceListId and faceIds should not be provided at the same time.
    /// </param>
    /// <param name="AMaxNumOfCandidatesReturned">
    ///   Optional parameter.
    ///   The number of top similar faces returned. The valid range is [1, 1000].It defaults to 20.
    /// </param>
    /// <param name="AFindMode">
    ///   Optional parameter.
    ///   Similar face searching mode. It can be "matchPerson" or "matchFace". It defaults to "matchPerson".
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an array of the most similar faces represented in faceId if the input parameter is faceIds or persistedFaceId if the input parameter is faceListId.
    ///   JSON fields in response body:
    ///   Fields	        Type	  Description
    ///   persistedFaceId	String	persistedFaceId of candidate face when find by faceListId. persistedFaceId in face list is persisted and will not expire. As showed in below response.
    ///   faceId	        String	faceId of candidate face when find by faceIds. faceId is created by Face - Detect and will expire 24 hours after the detection call.
    ///   confidence	    Number	Similarity confidence of the candidate face. The higher confidence, the more similar. Range between [0,1].
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function FindSimilar(const AFaceID: String; const AListID: String; const AMaxNumOfCandidatesReturned: Integer = 20; AFindMode: String = 'matchPerson'): String; overload;

    /// <summary>
    ///   Given query face's faceId, to search the similar-looking faces from a faceId array or a faceListId.
    ///   faceId array contains the faces created by Face - Detect, which will expire 24 hours after creation.
    ///   While "faceListId" is created by Face List - Create a Face List containing persistedFaceIds that will not expire.
    ///   Depending on the input the returned similar faces list contains faceIds or persistedFaceIds ranked by similarity.
    /// </summary>
    /// <param name="AFaceID">
    ///   faceId of the query face. User needs to call Face - Detect first to get a valid faceId.
    ///   Note that this faceId is not persisted and will expire 24 hours after the detection call.
    /// </param>
    /// <param name="AFaceIDS">
    ///   An array of candidate faceIds. All of them are created by Face - Detect and the faceIds will expire 24 hours after the detection call.
    ///   The number of faceIds is limited to 1000. Parameter faceListId and faceIds should not be provided at the same time.
    /// </param>
    /// <param name="AMaxNumOfCandidatesReturned">
    ///   Optional parameter.
    ///   The number of top similar faces returned. The valid range is [1, 1000].It defaults to 20.
    /// </param>
    /// <param name="AFindMode">
    ///   Optional parameter.
    ///   Similar face searching mode. It can be "matchPerson" or "matchFace". It defaults to "matchPerson".
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an array of the most similar faces represented in faceId if the input parameter is faceIds or persistedFaceId if the input parameter is faceListId.
    ///   JSON fields in response body:
    ///   Fields	        Type	  Description
    ///   persistedFaceId	String	persistedFaceId of candidate face when find by faceListId. persistedFaceId in face list is persisted and will not expire. As showed in below response.
    ///   faceId	        String	faceId of candidate face when find by faceIds. faceId is created by Face - Detect and will expire 24 hours after the detection call.
    ///   confidence	    Number	Similarity confidence of the candidate face. The higher confidence, the more similar. Range between [0,1].
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function FindSimilar(const AFaceID: String; AFaceIDS: TStringList; const AMaxNumOfCandidatesReturned: Integer = 20; AFindMode: String = 'matchPerson'): String; overload;

    /// <summary>
    ///   Divide candidate faces into groups based on face similarity.
    ///   The output is one or more disjointed face groups and a messyGroup. A face group contains faces that have similar looking, often of the same person.
    ///   Face groups are ranked by group size, i.e. number of faces. Notice that faces belonging to a same person might be split into several groups in the result.
    ///   MessyGroup is a special face group containing faces that cannot find any similar counterpart face from original faces. The messyGroup will not appear in the result if all faces found their counterparts.
    ///   Group API needs at least 2 candidate faces and 1000 at most. We suggest to try <see cref="uIFaceApi|IFaceApi.Verify">Face - Verify</see> when you only have 2 candidate faces.
    /// </summary>
    /// <param name="AFaceIDS">
    ///   Array of candidate faceId created by Face - Detect. The maximum is 1000 faces.
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns one or more groups of similar faces (rank by group size) and a messyGroup.
    ///   JSON fields in response body:
    ///   Fields	    Type	  Description
    ///   groups	    Array	  A partition of the original faces based on face similarity. Groups are ranked by number of faces.
    ///   messyGroup	Array	  Face ids array of faces that cannot find any similar faces from original faces.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function Group(AFaceIDS: TStringList): String;

    /// <summary>
    ///   Identify unknown faces from a person group.
    ///   For each face in the faceIds array, Face Identify will compute similarities between the query face and all the faces in the person group (given by personGroupId), and returns candidate person(s) for that face ranked by similarity confidence.
    ///   The person group should be trained to make it ready for identification.
    ///   Remarks:
    ///     The algorithm allows more than one face to be identified independently at the same request, but the no more than 10 faces.
    ///     Each person in the person group could have more than one face, but no more than 248 faces.
    ///     Identification works well for frontal faces and near-frontal faces.
    ///     Number of candidates returned is restricted by maxNumOfCandidatesReturned and confidenceThreshold. If no person is identified, the candidate returned will be an empty array.
    ///     Try Face - Find Similar when you need to identify similar faces from a face list instead of a person group.
    /// </summary>
    /// <param name="AFaceIDS">
    ///   Array of query faces faceIds, created by the Face - Detect.
    ///   Each of the faces are identified independently. The maximum number of faceIds is 10.
    /// </param>
    /// <param name="AGroupID">
    ///   Id of the target person group, created by Person Group - Create a Person Group.
    /// </param>
    /// <param name="AMaxNumOfCandidatesReturned">
    ///   Maximum number of returned candidates for each face is between 1 and 5 (default is 1).
    /// </param>
    /// <param name="AConfidenceThreshold">
    ///   Optional parameter.
    ///   Confidence threshold of identification, used to judge whether one face belong to one person.
    ///   The range of confidenceThreshold is [0, 1] (default specified by algorithm or by developer :)).
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns the identified candidate person(s) for each query face.
    ///   JSON fields in response body:
    ///   Fields	    Type	  Description
    ///   faceId	    String	faceId of the query face.
    ///   candidates	Array	  Identified person candidates for that face (ranked by confidence). Array size should be no larger than input maxNumOfCandidatesReturned. If no person is identified, will return an empty array.
    ///   personId	  String	personId of candidate person.
    ///   confidence	Number	A float number between 0.0 and 1.0.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function Identify(AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer = 1; const AConfidenceThreshold: Double = 0.5): String;

    /// <summary>
    ///   Verify whether two faces belong to a same person
    ///   Remarks:
    ///     This API works well for frontal and near-frontal faces.
    ///     For the scenarios that are sensitive to accuracy please make your own judgment.
    /// </summary>
		/// <param name="AFaceTempID1">
    ///     Temp Face Id of one face, comes from Face - Detect.
		/// </param>
		/// <param name="AFaceTempID2">
    ///     Temp Cace Id of another face, comes from Face - Detect.
		/// </param>
    function Verify(const AFaceTempID1, AFaceTempID2: String): String; overload;
    /// <summary>
    ///   Verify whether one face belongs to a person
    /// </summary>
		/// <param name="AFaceTempID">
    ///     Temp Face Id of one face, comes from Face - Detect.
		/// </param>
		/// <param name="APersonID">
    ///   Specify a certain person in a person group. personId is created in <see cref="uIFaceApi|IFaceApi.CreatePerson">Person - Create a Person</see>.
    /// </param>
    /// <param name="AGroupID">
    ///   Using existing personGroupId
		/// </param>
    function Verify(const AFaceTempID, APersonID, AGroupID: String): String; overload;
  end;

implementation

end.
