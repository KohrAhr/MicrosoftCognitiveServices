unit uFaceApi.Servers.Types;

interface

type
	/// <summary>
	///  	Face API V1.0 supported servers
	/// </summary>
	TFaceApiServer = (
		fasGeneral,

		fasWestUS,
		fasWestUS2,

		fasEastUS,
		fasEastUS2,

		fasWestCentralUS,
		fasSouthCentralUS,

		fasWestEurope,
		fasNorthEurope,

		fasSoutheastAsia,
		fasEastAsia,

		fasAustraliaEast,

		fasBrazilSouth,

		fasCanadaCentral,

		fasCentralIndia,

		fasUKSouth,

		fasJapanEast
	);

const

	CONST_FACE_API_MAIN_SERVER = 'api.cognitive.microsoft.com';

	CONST_FACE_API_SERVER_URLS: array [TFaceApiServer] of string = (
		CONST_FACE_API_MAIN_SERVER,
		'westus.' + CONST_FACE_API_MAIN_SERVER,
		'westus2.' + CONST_FACE_API_MAIN_SERVER,
		'eastus.' + CONST_FACE_API_MAIN_SERVER,
		'eastus2.' + CONST_FACE_API_MAIN_SERVER,
		'westcentralus.' + CONST_FACE_API_MAIN_SERVER,
		'southcentralus.' + CONST_FACE_API_MAIN_SERVER,
		'westeurope.' + CONST_FACE_API_MAIN_SERVER,
		'northeurope.' + CONST_FACE_API_MAIN_SERVER,
		'southeastasia.' + CONST_FACE_API_MAIN_SERVER,
		'eastasia.' + CONST_FACE_API_MAIN_SERVER,
		'australiaeast.' + CONST_FACE_API_MAIN_SERVER,
		'brazilsouth.' + CONST_FACE_API_MAIN_SERVER,
		'canadacentral.' + CONST_FACE_API_MAIN_SERVER,
		'centralindia.' + CONST_FACE_API_MAIN_SERVER,
		'uksouth.' + CONST_FACE_API_MAIN_SERVER,
		'japaneast.' + CONST_FACE_API_MAIN_SERVER
	);

implementation

end.
