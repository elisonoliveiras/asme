
' For selected output case this program calculates and creates new output vectors:
'	Beams:
'		Beam EndA Axial Stress
'		Beam EndB Axial Stress

'	Plates:
'		Plate Membrane X Normal Stress
'		Plate Membrane Y Normal Stress
'		Plate Membrane XY Shear Stress
'		Plate Membrane MajorPrn Stress
'		Plate Membrane MinorPrn Stress
'		Plate Membrane Stress Intensity
'		Plate Membrane VonMises Stress
' 		Plate Top Stress Intensity
'		Plate Bot Stress Intensity
'		Plate Top Triaxial Stress
'		Plate Bot Triaxial Stress

'	Solids:
'		Solid Stress Intensity
'		Solid Triaxial Stress 

' 	Reference ASME Section VIII, Div. 2, Appendix 4.
'
'
'	*****
'	Updated February 2020 to fix issues with calculation of Stress Intensities and Triaxial Stresses

'	Updated September 2019 to replace the use of the feOutput Set and feOutput object
'	for reading and writing results with the feResultsBrowsing object
'
'	Please note that the prior version of this API script overwrote the existing Output Vectors for the
'	Plate Top and Bottom Mean Stress (Output Vectors 7030 and 7430)
'	with the Plate Top and Plate Bottom Triaxial Stress Output Calculation.
'	This script does not ovewrite those vectors, but instead creates new Output vectors as noted in the
'       in the comments for each subroutine. The Output Vector IDs lie in the range of the new
'       ranges for User Defined Output Vectors from 24,000,000 to 24,999,999 24000000
'
'	Subroutines:
'
'		General Use:	PopulateRBOWithCornerResults	(used for Plate and Solid Elements)
'			
'
'		Beam Axial Stresses ==> CalculateBeamStressIntensities
'		
'		Plate Membrane X, Y and XY Shear Stresses ==> CalculatePlateMembraneStresses
'		Plate Membrane Major and Minor Principal Stresses ==> CalculatePlateMembranePrincipalStresses
'		Plate Membrane VonMises Stresses ==> CalculatePlateMembraneVonMisesStress
'		Plate Membrane Stress Intensities ==> CalculatePlateMembraneStressIntensities
'		Plate Top and Bottom Stress Intensities ==> CalculatePlateSurfaceStressIntensities
'		Plate Top and Bottom Triaxial Stresses ==> CalculatePlateTriaxialStresses
'
'		Solid Stress Intensities ==> CalculateSolidStressIntensities
'		Solid Triaxial Stresses ==> CalculateSolidTriaxialStresses
'
'	*****

Dim App As femap.model
Dim rc As femap.zReturnCode
Dim msgString As String

'	Output Set Object
Dim outputSetID As Long

'	Results Browsing Object (replaces Output Vector Object)
Dim RBO As femap.Results
Dim nColumnsAdded As Long	'Number of Output Columns Added
Dim columnIndicies As Variant	'Indices of Columns Added
Dim columnIndex As Long		'RBO Column Index for new Output Vector
Dim nColAdded As Long
Dim nColAdd1 As Long
Dim nColAdd2 As Long
Dim colIndicies As Variant
Dim colIndicies1 As Variant
Dim colIndicies2 As Variant
Dim newColumn() As Long
Dim newColumns As Variant

'	Sets for Beam, Plate and Solid Elements
Dim beamElemSet As femap.Set
Dim nBeams As Long
Dim plateElemSet As femap.Set
Dim nPlates As Long
Dim solidElemSet As femap.Set
Dim nSolids As Long

'	Element
Dim elem As femap.Elem
Dim elemID As Long
Dim elementID() As Long
Dim elementIDs As Variant

'	Element Property
Dim elemProp As femap.Prop
Dim elemPropID As Long

'	Plate and Solid Element Data
Dim ovID As Long	'	Output Vector ID
Dim cID(8) As Long
Dim cornerIDs As Variant
Dim ovTitle As String
Dim ovIndex As Long
Dim cIndex(8) As Long

'	Element Corner Results
Dim c1Result() As Double
Dim c1Results As Variant
Dim c1TopResults As Variant
Dim c1BotResults As Variant
Dim c2Result() As Double
Dim c2Results As Variant
Dim c2TopResults As Variant
Dim c2BotResults As Variant
Dim c3Result() As Double
Dim c3Results As Variant
Dim c3TopResults As Variant
Dim c3BotResults As Variant
Dim c4Result() As Double
Dim c4Results As Variant
Dim c4TopResults As Variant
Dim c4BotResults As Variant
Dim c5Result() As Double
Dim c5Results As Variant
Dim c5TopResults As Variant
Dim c5BotResults As Variant
Dim c6Result() As Double
Dim c6Results As Variant
Dim c6TopResults As Variant
Dim c6BotResults As Variant
Dim c7Result() As Double
Dim c7Results As Variant
Dim c7TopResults As Variant
Dim c7BotResults As Variant
Dim c8Result() As Double
Dim c8Results As Variant
Dim c8TopResults As Variant
Dim c8BotResults As Variant
Dim eResult() As Double
Dim eResults As Variant
Dim eTopResults As Variant
Dim eBotResults As Variant

'	Max, Min, Corner and Variables for Stress Intensity Calculations
Dim max() As Double
Dim min() As Double
Dim internal() As Double
Dim corner() As Double
Dim A As Double
Dim B As Double
Dim cen(2) As Double
Dim diff As Double

'
'	X, Y and XY Component Results
'
'	Plate X Membrane Stress
Dim xms As Variant
Dim xms1 As Variant
Dim xms2 As Variant
Dim xms3 As Variant
Dim xms4 As Variant
Dim xms5 As Variant
Dim xms6 As Variant
Dim xms7 As Variant
Dim xms8 As Variant
'	Plate Y Membrane Stress
Dim yms As Variant
Dim yms1 As Variant
Dim yms2 As Variant
Dim yms3 As Variant
Dim yms4 As Variant
Dim yms5 As Variant
Dim yms6 As Variant
Dim yms7 As Variant
Dim yms8 As Variant
'	Plate XY Membrane Shear Stress
Dim xyms As Variant
Dim xyms1 As Variant
Dim xyms2 As Variant
Dim xyms3 As Variant
Dim xyms4 As Variant
'	Plate Top Major Principal Stress
Dim ptmajps As Variant
Dim ptmajps1 As Variant
Dim ptmajps2 As Variant
Dim ptmajps3 As Variant
Dim ptmajps4 As Variant
'	Plate Bottom Major Principal Stress
Dim pbmajps As Variant
Dim pbmajps1 As Variant
Dim pbmajps2 As Variant
Dim pbmajps3 As Variant
Dim pbmajps4 As Variant
'	Plate Membrane Major Principal Stress
Dim pmmajps As Variant
Dim pmmajps1 As Variant
Dim pmmajps2 As Variant
Dim pmmajps3 As Variant
Dim pmmajps4 As Variant
'	Plate Membrane Minor Principal Stress
Dim pmminps As Variant
Dim pmminps1 As Variant
Dim pmminps2 As Variant
Dim pmminps3 As Variant
Dim pmminps4 As Variant
'	Plate Top Minor Principal Stress
Dim ptminps As Variant
Dim ptminps1 As Variant
Dim ptminps2 As Variant
Dim ptminps3 As Variant
Dim ptminps4 As Variant
'	Plate Bottom Minor Principal Stress
Dim pbminps As Variant
Dim pbminps1 As Variant
Dim pbminps2 As Variant
Dim pbminps3 As Variant
Dim pbminps4 As Variant
'	Plate Membrane VonMises Stress
Dim pmvms As Variant
Dim pmvms1 As Variant
Dim pmvms2 As Variant
Dim pmvms3 As Variant
Dim pmvms4 As Variant
'	Plate Membrane Stress Intensity
Dim pmsti As Variant
Dim pmsti1 As Variant
Dim pmsti2 As Variant
Dim pmsti3 As Variant
Dim pmsti4 As Variant
'	Plate Top Stress Intensity
Dim ptsti As Variant
Dim ptsti1 As Variant
Dim ptsti2 As Variant
Dim ptsti3 As Variant
Dim ptsti4 As Variant
'	Plate Bottom Stress Intensity
Dim pbsti As Variant
Dim pbsti1 As Variant
Dim pbsti2 As Variant
Dim pbsti3 As Variant
Dim pbsti4 As Variant


'	Major Principal Stress
Dim majps As Variant
Dim majps1 As Variant
Dim majps2 As Variant
Dim majps3 As Variant
Dim majps4 As Variant
Dim majps5 As Variant
Dim majps6 As Variant
Dim majps7 As Variant
Dim majps8 As Variant
'	Minor Principal Stress
Dim minps As Variant
Dim minps1 As Variant
Dim minps2 As Variant
Dim minps3 As Variant
Dim minps4 As Variant
Dim minps5 As Variant
Dim minps6 As Variant
Dim minps7 As Variant
Dim minps8 As Variant
'	Internal Principal Stress
Dim intps As Variant
Dim intps1 As Variant
Dim intps2 As Variant
Dim intps3 As Variant
Dim intps4 As Variant
Dim intps5 As Variant
Dim intps6 As Variant
Dim intps7 As Variant
Dim intps8 As Variant
'	VonMises Stress
Dim mvms1 As Variant
Dim mvms2 As Variant
Dim mvms3 As Variant
Dim mvms4 As Variant
Dim mvms As Variant

'	Solid Stress Intensities
Dim si As Variant
Dim si1 As Variant
Dim si2 As Variant
Dim si3 As Variant
Dim si4 As Variant

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub Main
	Set App = feFemap()
	Set RBO = App.feResults

'	Flag User Warning that Attached Results Do Not Work
	App.feAppMessageBox ( 0, "Note, this script does not work with Attached Results Sets!" )

'	Select Output Sets for Processing Results
	Dim outputSets As femap.Set
	Set outputSets = App.feSet
	rc = outputSets.Select ( 28 ,True, "Select Output Sets for Data Processing" )
	If rc <> fe_ok Then
		App.feAppMessage ( FCM_WARNING, "" )
		App.feAppMessage ( FCM_WARNING, "*****" )
		App.feAppMessage ( FCM_WARNING, "No Output Sets Selected, Exiting Macro" )
		App.feAppMessage ( FCM_WARNING, "*****" )
		App.feAppMessage ( FCM_WARNING, "" )
		End
	End If

	rc = App.feAppMessage( 0, "Number of Selected Output Sets =" + Str(outputSets.Count))

'	**************************************************
'	Get Sets of Beam, LInear Plate and Linear Solid Elements
'	**************************************************
	Set beamElemSet = App.feSet
	Set plateElemSet = App.feSet
	Set solidElemSet = App.feSet

	Set elem = App.feElem
	Set elemProp = App.feProp

	While elem.Next
		If elem.type = FET_L_BEAM Then
			beamElemSet.Add ( elem.ID )
		ElseIf elem.Type = FET_L_PLATE Then
			plateElemSet.Add ( elem.ID )
		ElseIf elem.Type = FET_L_SOLID Then
			solidElemSet.Add ( elem.ID )
		End If
	Wend

	nBeams = beamElemSet.Count
	nPlates = plateElemSet.Count
	nSolids = solidElemSet.Count
	Dim count As Long
	count = outputSets.Count  * ( (nBeams * 2) + ( nPlates * 11) + ( nSolids * 2 ) )

	outputSets.Reset
	outputSetID = outputSets.First
	For i = 0 To outputSets.Count-1
		App.feAppMessage ( FCM_HIGHLIGHT, "" )
		App.feAppMessage ( FCM_HIGHLIGHT, _
			("Output Set" + Str$(outputSetID)) )

		'Calculate Beam Stress Intensities
		If beamElemSet.Count > 0 Then
			App.feAppMessage ( FCM_HIGHLIGHT, "   Processing Beam Stresses" )
			CalculateBeamAxialStresses ( )
		End If

		'Calculate Plate Element Stresses
		If plateElemSet.Count > 0 Then
			ReDim elementID(nPlates)
			plateElemSet.Reset
			elemID = plateElemSet.First
			For n = 0 To nPlates-1
				elementID(n) = elemID
				elemID = plateElemSet.Next
			Next n
			elementIDs = elementID

			cID(4) = 0
			cID(5) = 0
			cID(6) = 0
			cID(7) = 0

			ReDim c1Result(nPlates)
			ReDim c2Result(nPlates)
			ReDim c3Result(nPlates)
			ReDim c4Result(nPlates)
			ReDim c5Result(nPlates)
			ReDim c6Result(nPlates)
			ReDim c7Result(nPlates)
			ReDim c8Result(nPlates)
			ReDim eResult(nPlates)
			ReDim newColumn(4)

			ReDim max(4)
			ReDim min(4)
			ReDim corner(4)

			App.feAppMessage ( FCM_HIGHLIGHT, "   Processing Plate Stresses" )

			CalculatePlateMembraneStresses ( )
			CalculatePlateMembranePrincipalStresses ( )
			CalculatePlateMembraneVonMisesStresses ( )
			CalculatePlateStressIntensities ( )
			CalculatePlateTriaxialStresses ( )
		End If

		'Calculate Solid Stress Intensities
		If solidElemSet.Count > 0 Then
			App.feAppMessage ( FCM_HIGHLIGHT, "   Processing Solid Element Stresses" )

			ReDim elementID(nSolids)
			solidElemSet.Reset
			elemID = solidElemSet.First
			For n = 0 To nSolids-1
				elementID(n) = elemID
				elemID = solidElemSet.Next
			Next n
			elementIDs = elementID

			ReDim c1Result(nSolids)
			ReDim c2Result(nSolids)
			ReDim c3Result(nSolids)
			ReDim c4Result(nSolids)
			ReDim c5Result(nSolids)
			ReDim c6Result(nSolids)
			ReDim c7Result(nSolids)
			ReDim c8Result(nSolids)
			ReDim eResult(nSolids)
			ReDim newColumn(8)

			ReDim max(8)
			ReDim min(8)
			ReDim internal(8)
			ReDim corner(8)

			CalculateSolidStressIntensities ( )
			CalculateSolidTriaxialStresses ( )
		End If

		outputSetID = outputSets.Next
	Next i

	rc = App.feAppMessage ( 0, "" )
	rc = App.feAppMessage ( FCM_HIGHLIGHT, "ASME Stress Intensity Calculations Complete" )
	App.feViewRegenerate ( 0 )

End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~
'	 B E A M    A X I A L   S T R E S S
'~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculateBeamAxialStresses ( )
	Dim area() As Double	'Beam Cross Sectional Area
	Dim pA() As Double	'Beam End A Axial Force
	Dim beamAxialStressA() As Double	'Beam End A Axial Stress
	Dim beamAxialStressesA As Variant
	Dim pB() As Double	'Beam End B Axial Force
	Dim beamAxialStressB() As Double	'Beam End B Axial Stress
	Dim beamAxialStressesB As Variant

	ReDim elementID(nBeams)
	beamElemSet.Reset
	elemID = beamElemSet.First
	For ne = 0 To nBeams-1
		elementID(ne) = elemID
		elemID = beamElemSet.Next
	Next ne
	elementIDs = elementID

	ReDim area(nBeams)
	ReDim pA(nBeams)
	ReDim beamAxialStressA(nBeams)
	ReDim pB(nBeams)
	ReDim beamAxialStressB(nBeams)

'	**************************************************************************
'
'	Create Output Vectors 24003164 and 24003166
'		Beam End A Axial Stress (24003164) =
'			Beam End A Axial Force (3022) / Beam Cross Section Area 
'
'		Beam End B Axial Stress (24003166) =
'			Beam End B Axial Force (3023) / Beam Cross Section Area
'
'	**************************************************************************
	RBO.Clear	'Reset the RBO
	Dim nColAdd1 As Long
	Dim colIndices1 As Variant
	Dim nColAdd2 As Long
	Dim colIndices2 As Variant
	Dim newColumns As Variant
	rc = RBO.AddColumnV2 ( OutputSetID, 3022, False, nColAdd1, colIndices1 )
	rc = RBO.AddColumnV2 ( OutputSetID, 3023, False, nColAdd1, colIndices2 )
	RBO.Populate

	For i = 0 To nBeams-1
		elem.Get ( elementIDs(i) )
		elemPropID = elem.propID
		elem.Put ( elementIDs(i) )

		elemProp.Get ( elemPropID )
		area(i) = elemProp.pval(0)
		elemProp.Put ( elemPropID )

		'Get Values of  the Beam End Axial Forces from the RBO
		RBO.GetValue ( elementIDs(i), colIndices1(0), pA(i) )
		beamAxialStressA(i) = pA(i)/area(i)

		RBO.GetValue ( elementIDs(i), colIndices2(0), pB(i) )
		beamAxialStressB(i) = pB(i)/area(i)
	Next i

	beamAxialStressesA = beamAxialStressA
	beamAxialStressesB = beamAxialStressB

	RBO.Clear
	rc = RBO.AddScalarAtBeamColumnsV2 ( outputSetID, 24003164, 24003166, "Beam Axial Stress", _
		2, FOT_STRESS, False, True, newColumns )
	rc = RBO.SetColumn ( newColumns(0), nBeams, elementIDs,beamAxialStressesA )
	rc = RBO.SetColumn ( newColumns(1), nBeams, elementIDs,beamAxialStressesB )
	RBO.Save

End Sub


Sub PopulateRBOWithCornerResults ( )
	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 ( outputSetID, ovID, _
		cID(0), cID(1), cID(2), cID(3), cID(4), cID(5), cID(6), cID(7), _
		ovTitle, FOT_STRESS, False, columnIndicies )
	rc = RBO.Populate
End Sub


'	~~~~~~~~~~~~~~~~~
'	Plate Membrane Stresses
'	~~~~~~~~~~~~~~~~~
Sub CalculatePlateMembraneStresses ( )
	rc = RBO.VectorExistsV2 ( outputSetID, 7020 )
	If rc <> fe_OK Then
		App.feAppMessage ( FCM_WARNING, "" )
		App.feAppMessage ( FCM_WARNING, "*****" )
		App.feAppMessage ( FCM_WARNING, _
			("Plate Stress Output Data Missing for Output Set" + Str$(outputSetID)) )
		App.feAppMessage ( FCM_WARNING, "*****" )
		App.feAppMessage ( FCM_WARNING, "" )
		GoTo EndPlateStressCalculations
	End If

'	*****************************************
'
'	Calculate Plate Membrane X Normal Stress
'		Plate Plate Membrane X Normal Stress =
'			( PlateTopXNormalStress+ PlateBottomXNormalStress) / 2.0
'		
'		Output Vector IDs:
'			Plate X Membrane Stress - 24008020 (formerly 30000)
'			Plate C1 X Membrane Stress - 24080201 (formerly 101)
'			Plate C2 X Membrane Stress - 24080202 (formerly 102)
'			Plate C3 X Membrane Stress - 24080203 (formerly 103)
'			Plate C4 X Membrane Stress - 24080204 (formerly 104)
'
'	*****************************************
'	Get Plate Top X Normal Stress
	ovID = 7020
	cID(0) = 11020
	cID(1) = 15020
	cID(2) = 19020
	cID(3) = 23020
	cID(4) = 0
	cID(5) = 0
	cID(6) = 0
	cID(7) = 0
	cornerIDs = cID
	ovTitle = "Plate Top X Normal Stress"
	PopulateRBOWithCornerResults ( )

	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	eTopResults = eResult
	c1TopResults = c1Result
	c2TopResults = c2Result
	c3TopResults = c3Result
	c4TopResults = c4Result

'	Get Plate Bottom X Normal Stress
	RBO.clear
	ovID = 9020
	cID(0) = 13020
	cID(1) = 17020
	cID(2) = 21020
	cID(3) = 25020
	cID(4) = 0
	cID(5) = 0
	cID(6) = 0
	cID(7) = 0
	cornerIDs = cID
	ovTitle = "Plate Membrane X Normal Stress"
	PopulateRBOWithCornerResults ( )

	For i = 0 To nPlates-1
		RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	eBotResults = eResult
	c1BotResults = c1Result
	c2BotResults = c2Result
	c3BotResults = c3Result
	c4BotResults = c4Result
'
'	Calculate X Membrane Stress
'	
	CalculateAverageStress ( )
	xms = eResults
	xms1= c1Results
	xms2 = c2Results
	xms3 = c3Results
	xms4 = c4Results

	RBO.clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24008020, 24080201, 24080202, 24080203, 24080204, 0, 0, 0, 0, _
		"Plate Membrane X Normal Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, xms )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, xms1 )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, xms2 )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, xms3 )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, xms4 )
	RBO.Save

'	*****************************************
'
'	Calculate Plate Plate Membrane Y Normal Stress
'		Plate Plate Membrane X Normal Stress =
'			( PlateBottomYNormalStress+ PlateBottomYNormalStress) / 2.0
'		
'		Output Vector IDs:
'			Plate Y Membrane Stress - 24008021 (formerly 30005)
'			Plate C1 Y Membrane Stress - 24080211 (formerly 105)
'			Plate C2 Y Membrane Stress - 24080212 (formerly 106)
'			Plate C3 Y Membrane Stress - 24080213(formerly 107)
'			Plate C4 Y Membrane Stress - 24080214 (formerly 108)
'
'	*****************************************
'	Get Plate Top Y Normal Stress
	ovID = 7021
	cID(0) = 11021
	cID(1) = 15021
	cID(2) = 19021
	cID(3) = 23021
	cornerIDs = cID
	ovTitle = "Plate Top Y Normal Stress"
	PopulateRBOWithCornerResults ( )

	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )

	Next i
	eTopResults = eResult
	c1TopResults = c1Result
	c2TopResults = c2Result
	c3TopResults = c3Result
	c4TopResults = c4Result
'
'	Get Plate Bottom Y Normal Stress
'
	RBO.clear
	ovID = 9021
	cID(0) = 13021
	cID(1) = 17021
	cID(2) = 21021
	cID(3) = 25021
	cornerIDs = cID
	ovTitle = "Plate Bottom Y Normal Stress"

	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	eBotResults = eResult
	c1BotResults = c1Result
	c2BotResults = c2Result
	c3BotResults = c3Result
	c4BotResults = c4Result
'	
'	Calculate Y Membrane Stress
'	
	CalculateAverageStress ( )
	yms = eResult
	yms1 = c1Result
	yms2 = c2Result
	yms3 = c3Result
	yms4 = c4Result

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24008021, 24080211, 24080212, 24080213, 24080214, 0, 0, 0, 0, _
		"Plate Membrane Y Normal Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, yms )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, yms1 )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, yms2 )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, yms3 )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, yms4 )
	RBO.Save

'
'	*****************************************
'
'	Calculate Plate Membrane XY Shear Stress
'		Plate XY Membrane Stress = ( PlateTopXYShearStress+ PlateBottomXYShearStress) / 2.0
'		
'		Output Vector IDs:
'			Plate X Membrane Stress - 24008023
'			Plate C1 X Membrane Stress - 24080231 (formerly 105)
'			Plate C2 X Membrane Stress - 24080232 (formerly 106)
'			Plate C3 X Membrane Stress - 24080233 (formerly 107)
'			Plate C4 X Membrane Stress - 24080234 (formerly 108)
'
'	*****************************************
'	Get Plate Top XY Shear Stress
	ovID = 7023
	cID(0) = 11023
	cID(1) = 15023
	cID(2) = 19023
	cID(3) = 23023
	cornerIDs = cID
	ovTitle = "Plate Top XY Shear Stress"

	RBO.Clear
	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	eTopResults = eResult
	c1TopResults = c1Result
	c2TopResults = c2Result
	c3TopResults = c3Result
	c4TopResults = c4Result

'	Get Plate Bottom XY Shear Stress
	ovID = 9023
	cID(0) = 13023
	cID(1) = 17023
	cID(2) = 21023
	cID(3) = 25023
	cornerIDs = cID
	ovTitle = "Plate Bottom XY Shear Stress"

	RBO.Clear
	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	eBotResults = eResult
	c1BotResults = c1Result
	c2BotResults = c2Result
	c3BotResults = c3Result
	c4BotResults = c4Result

'
'	Calculate XY Membrane Shear Stress
'	
	CalculateAverageStress ( )

	RBO.clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24008023, 24080231, 24080232, 24080233, 24080234, 0, 0, 0, 0, _
		"Plate Membrane XY Shear Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, c4Results )
	RBO.Save

	xyms = eResults
	xyms1= c1Results
	xyms2 = c2Results
	xyms3 = c3Results
	xyms4 = c4Results

	EndPlateStressCalculations:

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'	Calculate Plate Membrane Principal Stresses
'		Plate Membrane Major Principal Stress =
'			(Plate Membrane X Stress + Plate Membrane Y Stress) / 2.0) +
'			(Sqr ( ((Plate Membrane X Stress - Plate Membrane Y Stress)/2.0)^2.) +
'					 (Plate XY Membrane Shear Stress ^ 2.) ) )
'		
'		Output Vector IDs:
'			Plate Membrane Major Principal Stress - 24008026 (formerly 300150)
'			Plate C1 X Membrane Major Principal Stress - 24080261 (formerly 115)
'			Plate C2 X Membrane Major Principal Stress - 24080262 (formerly 116)
'			Plate C3 X Membrane Major Principal Stress - 24080263 (formerly 117)
'			Plate C4 X Membrane Major Principal Stress - 24080264 (formerly 118)
'
'		Plate Membrane Minor Principal Stress =
'			(Plate Membrane X Stress + Plate Membrane Y Stress) / 2.0) -
'			(Sqr ( (Plate Membrane X Stress - Plate Membrane Y Stress)/2.0)^2.) +
'					 (Plate XY Membrane Shear Stress ^ 2.) ) )
'		
'		Output Vector IDs:
'			Plate Membrane Minor Principal Stress - 24008027 (formerly 300020)
'			Plate C1 X Membrane Minor Principal Stress - 24080271 (formerly 120)
'			Plate C2 X Membrane Minor Principal Stress - 24080272 (formerly 121)
'			Plate C3 X Membrane Minor Principal Stress - 24080273 (formerly 122)
'			Plate C4 X Membrane Minor Principal Stress - 24080274 (formerly 123)
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculatePlateMembranePrincipalStresses ( )
'
'	Plate Membrane Major Principal Stresses
'
	For i = 0 To nPlates-1
		c1Result(i) = ( ( xms1(i) + yms1(i) ) / 2.0 ) + Sqr ( ( ( ( xms1(i) - yms1(i) ) / 2. ) ^ 2. ) +	( xyms1(i) ^ 2.0 ) )
		c2Result(i) = ( ( xms2(i) + yms2(i) ) / 2.0 ) + Sqr ( ( ( ( xms2(i) - yms2(i) ) / 2. ) ^ 2. ) +	( xyms2(i) ^ 2.0 ) )
		c3Result(i) = ( ( xms3(i) + yms3(i) ) / 2.0 ) + Sqr ( ( ( ( xms3(i) - yms3(i) ) / 2. ) ^ 2. ) + ( xyms3(i) ^ 2.0 ) )
		c4Result(i) = ( ( xms4(i) + yms4(i) ) / 2.0 ) + Sqr ( ( ( ( xms4(i) - yms4(i) ) / 2. ) ^ 2. ) +	 (xyms4(i) ^ 2.0 ) )
		eResult(i) = ( ( xms(i) + yms(i) ) / 2.0 ) + Sqr ( ( ( ( xms(i) - yms(i) )/ 2. ) ^ 2. ) + ( xyms(i) ^ 2.0) )
	Next i
	pmmajps = eResult
	pmmajps1 = c1Result
	pmmajps2 = c2Result
	pmmajps3 = c3Result
	pmmajps4 = c4Result


	RBO.clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24008026, 24080261, 24080262, 24080263, 24080264, 0, 0, 0, 0, _
		"Plate Membrane Major Principal Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, pmmajps )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, pmmajps1 )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, pmmajps2 )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, pmmajps3 )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, pmmajps4 )
	RBO.Save

'
'	Plate Membrane Minor Principal Stresses
'
	For i = 0 To nPlates-1
		c1Result(i) = ( ( xms1(i) + yms1(i) ) / 2.0 ) - Sqr ( ( ( ( xms1(i) - yms1(i) ) / 2. ) ^ 2. ) +	( xyms1(i) ^ 2.0 ) )
		c2Result(i) = ( ( xms2(i) + yms2(i) ) / 2.0 ) - Sqr ( ( ( ( xms2(i) - yms2(i) ) / 2. ) ^ 2. ) +	( xyms2(i) ^ 2.0 ) )
		c3Result(i) = ( ( xms3(i) + yms3(i) ) / 2.0 ) - Sqr ( ( ( ( xms3(i) - yms3(i) ) / 2. ) ^ 2. ) + ( xyms3(i) ^ 2.0 ) )
		c4Result(i) = ( ( xms4(i) + yms4(i) ) / 2.0 ) - Sqr ( ( ( ( xms4(i) - yms4(i) ) / 2. ) ^ 2. ) +	 (xyms4(i) ^ 2.0 ) )
		eResult(i) = ( ( xms(i) + yms(i) ) / 2.0 ) - Sqr ( ( ( ( xms(i) - yms(i) )/ 2. ) ^ 2. ) + ( xyms(i) ^ 2.0) )
	Next i
	pmminps = eResult
	pmminps1 = c1Result
	pmminps2 = c2Result
	pmminps3 = c3Result
	pmminps4 = c4Result

	RBO.clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24008027, 24080271, 24080272, 24080273, 24080274, 0, 0, 0, 0, _
		"Plate Membrane Minor Principal Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, pmminps )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, pmminps1 )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, pmminps2 )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, pmminps3 )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, pmminps4 )
	RBO.Save

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'	Calculate Plate Membrane VonMises Stress
'		Plate Membrane Major Principal Stress =
'			(Plate Membrane X Stress + Plate Membrane Y Stress) / 2.0) +
'			(Sqr ( (Plate Membrane X Stress - Plate Membrane Y Stress)/2.0)^2.) +
'					 (Plate XY Membrane Shear Stress ^ 2.) ) )
'		
'		Output Vector IDs:
'			Plate Membrane VonMises Stress - 24008033 (formerly 300050)
'			Plate C1 Membrane VonMises Stress - 24080331 (formerly 115)
'			Plate C2 Membrane VonMises Stress - 24080332 (formerly 116)
'			Plate C3 Membrane VonMises Stress - 24080333 (formerly 117)
'			Plate C4 Membrane VonMises Stress - 24080334 (formerly 118)
'
'		Plate Membrane Von Mises Stress =
'			(Sqr ( (Plate Membrane Major Principal Stress ^ 2.) - ( Plate Membrane Principal Stress - Plate Membrane Minor Principal Stress ) + 
'					 (Plate Membrane Minor Principal Stress ^ 2.)  ) )
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculatePlateMembraneVonMisesStresses ( )
'
'	Plate Membrane Von Mises Stresses
'
	For i = 0 To nPlates-1
		eResult(i) = Sqr ( ( pmmajps(i) ^ 2.) - ( pmmajps(i) * pmminps(i) ) + ( pmminps(i) ^ 2. ) )
		c1Result(i) = Sqr ( ( pmmajps1(i) ^ 2.) - ( pmmajps1(i) * pmminps1(i) ) + ( pmminps1(i) ^ 2. ) )
		c2Result(i) = Sqr ( ( pmmajps2(i) ^ 2.) - ( pmmajps2(i) * pmminps2(i) ) + ( pmminps2(i) ^ 2. ) )
		c3Result(i) = Sqr ( ( pmmajps3(i) ^ 2.) - ( pmmajps3(i) * pmminps3(i) ) + ( pmminps3(i) ^ 2. ) )
		c4Result(i) = Sqr ( ( pmmajps4(i) ^ 2.) - ( pmmajps4(i) * pmminps4(i) ) + ( pmminps4(i) ^ 2. ) )
	Next i
	pmvms = eResult
	pmvms1 = c1Result
	pmvms2 = c2Result
	pmvms3 = c3Result
	pmvms4 = c4Result

	RBO.clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24008033, 24080331, 24080332, 24080333, 24080334, 0, 0, 0, 0, _
		"Plate Membrane VonMises Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, pmvms )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, pmvms1 )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, pmvms2 )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, pmvms3 )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, pmvms4 )
	RBO.Save

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'	Calculate Plate Membrane, Top and Plate Bottom Stress Intensities
'		
'		Output Vector IDs:
'
'			Plate Membrane Stress Intensity - 24017030
'			Plate C1 Membrane Stress Intensity - 24070301 (formerly 125)
'			Plate C2 Membrane Stress Intensity - 24070302 (formerly 126)
'			Plate C3 Membrane Stress Intensity - 24070303 (formerly 127)
'			Plate C4 Membrane Stress Intensity - 24070304 (formerly 128)
'
'			Plate Top Stress Intensity - 24017031 (formerly 7031)
'			Plate C1 Top Stress Intensity - 24170311 (formerly 125)
'			Plate C2 Top Stress Intensity - 24170312 (formerly 126)
'			Plate C3 Top Stress Intensity - 24170313 (formerly 127)
'			Plate C4 Top Stress Intensity - 24170314 (formerly 128)
'
'			Plate Bottom Stress Intensity - 24019031 (formerly 7431)
'			Plate C1 Bottom Stress Intensity - 24190311 (formerly 125)
'			Plate C2 Bottom Stress Intensity - 24190312 (formerly 126)
'			Plate C3 Bottom Stress Intensity - 24190313 (formerly 127)
'			Plate C4 Bottom Stress Intensity - 24190314 (formerly 128)
'
'
'		Stress Intensity is the Maximum Value of one of the three(3) following values:
'			Absolute Value of the Maximum Principal Stress (MaxPrin)
'			Absolute Value of the Minimum Principal Stress (MinPrin)
'			Absolute Value of the Difference between the MaxPrin and MinPrin
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculatePlateStressIntensities ( )
	
' ******************************************************
' *
' *		Calculate Plate Membrane Stress Intensity
' *
' ******************************************************
'	Plate Membrane Maximum Principal Stresses
	majps = pmmajps
	majps1 = pmmajps1
	majps2 = pmmajps2
	majps3 = pmmajps3
	majps4 = pmmajps4

'	Plate Membrane Minimum Principal Stresses
	minps = pmminps
	minps1 = pmminps1
	minps2 = pmminps2
	minps3 = pmminps3
	minps4 = pmminps4

	PlateStressIntensitiesCalculation ( )

	pmsti = eResults
	pmsti1 = c1Results
	pmsti2 = c2Results
	pmsti3 = c3Results
	pmsti4 = c4Results

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24017030, 24070301, 24070302, 24070303, 24070304, 0, 0, 0, 0, _
		"Plate Membrane Stress Intensity", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, c4Results )
	RBO.Save

' ******************************************************
' *
' *		Calculate Plate Top Stress Intensity
' *
' ******************************************************
'	Get Plate Top Maxium Principal Stress
	ovID = 7026
	cID(0) = 11026
	cID(1) = 15026
	cID(2) = 19026
	cID(3) = 23026
	cornerIDs = cID
	ovTitle = "Plate Top Maxium Principal Stress"

	RBO.Clear
	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	ptmajps = eResult
	ptmajps1 = c1Result
	ptmajps2 = c2Result
	ptmajps3 = c3Result
	ptmajps4 = c4Result

	majps = ptmajps
	majps1 = ptmajps1
	majps2 = ptmajps2
	majps3 = ptmajps3
	majps4 = ptmajps4

'	Get Plate Top Minor Principal Stress
	ovID = 7027
	cID(0) = 11027
	cID(1) = 15027
	cID(2) = 19027
	cID(3) = 23027
	cornerIDs = cID
	ovTitle = "Plate Top Minor Principal Stress"

	RBO.Clear
	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	ptminps = eResult
	ptminps1 = c1Result
	ptminps2 = c2Result
	ptminps3 = c3Result
	ptminps4 = c4Result

	minps = ptminps
	minps1 = ptminps1
	minps2 = ptminps2
	minps3 = ptminps3
	minps4 = ptminps4

	PlateStressIntensitiesCalculation ( )

	ptsti = eResults
	ptsti1 = c1Results
	ptsti2 = c2Results
	ptsti3 = c3Results
	ptsti4 = c4Results

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24017031, 24170311, 24170312, 24170313, 24170314, 0, 0, 0, 0, _
		"Plate Top Stress Intensity", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, c4Results )
	RBO.Save

' ******************************************************
' *
' *		Calculate Plate Bottom Stress Intensity
' *
' ******************************************************
'	Get Plate Bottom Maxium Principal Stress
	ovID = 9026
	cID(0) = 13026
	cID(1) = 17026
	cID(2) = 21026
	cID(3) = 26026
	cornerIDs = cID
	ovTitle = "Plate Top Maxium Principal Stress"

	RBO.Clear
	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	pbmajps = eResult
	pbmajps1 = c1Result
	pbmajps2 = c2Result
	pbmajps3 = c3Result
	pbmajps4 = c4Result

	majps = pbmajps
	majps1 = pbmajps1
	majps2 = pbmajps2
	majps3 = pbmajps3
	majps4 = pbmajps4

'	Get Plate Bottom Minor Principal Stress
	ovID = 9027
	cID(0) = 13027
	cID(1) = 17027
	cID(2) = 21027
	cID(3) = 25027
	cornerIDs = cID
	ovTitle = "Plate Top Minor Principal Stress"

	RBO.Clear
	PopulateRBOWithCornerResults ( )
	For i = 0 To nPlates-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
	Next i
	pbminps = eResult
	pbminps1 = c1Result
	pbminps2 = c2Result
	pbminps3 = c3Result
	pbminps4 = c4Result

	minps = pbminps
	minps1 = pbminps1
	minps2 = pbminps2
	minps3 = pbminps3
	minps4 = pbminps4

	PlateStressIntensitiesCalculation ( )

	pbsti = eResults
	pbsti1 = c1Results
	pbsti2 = c2Results
	pbsti3 = c3Results
	pbsti4 = c4Results

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24019031, 24190311, 24190312, 24190313, 24190314, 0, 0, 0, 0, _
		"Plate Bottom Stress Intensity", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, c4Results )
	RBO.Save

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'
'	Calculate Plate Stress Intensities
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub PlateStressIntensitiesCalculation ( )
	For i = 0 To nPlates-1
		max(0) = majps1(i)
		max(1) = majps2(i)
		max(2) = majps3(i)
		max(3) = majps4(i)

		min(0) = minps1(i)
		min(1) = minps2(i)
		min(2) = minps3(i)
		min(3) = minps4(i)

		cen(0) = majps(i)
		cen(1) = minps(i)

		'	Calculate Corner Stress Intensities
		For k = 0 To 3
			A = Abs ( max(k) )	'Absolute value of Major Principal Stress
			B = Abs ( min(k) )		'Absolute value of Minor Principal Stress
			diff = Abs ( max(k) - min(k) )		'Absolute value of (Major Principle Stress - Minor Principle Stress)
			If diff > A Then	'
				If diff > B Then
					corner(k) = diff
				Else
					corner(k) = B
				End If
			ElseIf A > B Then
				corner(k) = A
			ElseIf B > A Then
				corner(k) = B
			End If
		Next k

		'	Calculate Element Stress Intensities
		A = Abs ( cen(0) )
		B = Abs ( cen(1) )
		diff = Abs ( cen(0) - cen(1) )
		If diff > A Then
			If diff > B Then
				cen(0) = diff
			Else
				cen(0) = B
				End If
			ElseIf A > B Then
				cen(0) = A
			ElseIf B > A Then
				cen(0) = B
		End If

		c1Result(i) = corner(0)
		c2Result(i) = corner(1)
		c3Result(i) = corner(2)
		c4Result(i) = corner(3)
		eResult(i) = cen(0)
		
	Next i

	c1Results = c1Result
	c2Results = c2Result
	c3Results = c3Result
	c4Results = c4Result
	eResults = eResult

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'	Calculate Plate Top and Plate Triaxial Stresses
'
'		Triaxial Stress = Major Principal Stress + Minor Principal Stress
'		
'		Output Vector IDs:
'			Plate Top Triaxial Stress - 24007030 (formerly 7030)
'			Plate C1 Top Stress Intensity - 24070301 (formerly 145)
'			Plate C2 Top Stress Intensity - 24070302 (formerly 146)
'			Plate C3 Top Stress Intensity - 24070303 (formerly 147)
'			Plate C4 Top Stress Intensity - 24070304 (formerly 148)
'
'			Plate Bottom Triaxial Stress - 24009030 (formerly 7040)
'			Plate C1 Bottom Stress Intensity - 24090301 (formerly 151)
'			Plate C2 Bottom Stress Intensity - 24090302 (formerly 152)
'			Plate C3 Bottom Stress Intensity - 24090303 (formerly 153)
'			Plate C4 Bottom Stress Intensity - 24090301 (formerly 154)
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculatePlateTriaxialStresses ( )
'	Calculate Plate Top Triaxial Stresses
	For i = 0 To nPlates-1
		eResult(i) =  ptmajps(i)  + ptminps(i)
		c1Result(i) = ptmajps1(i) + ptminps1(i)
		c2Result(i) = ptmajps2(i)  + ptminps2(i)
		c3Result(i) = ptmajps3(i)  + ptminps3(i)
		c4Result(i) = ptmajps4(i)  + ptminps4(i)
	Next i
	eResults = eResult
	c1Results = c1Result
	c2Results = c2Result
	c3Results = c3Result
	c4Results = c4Result

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24007030, 24070301, 24070302, 24070303, 24070304, 0, 0, 0, 0, _
		"Plate Top Triaxial Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, c4Results )
	RBO.Save

	'	Calculate Plate Bottom Triaxial Stresses
	For i = 0 To nPlates-1
		eResult(i) =  pbmajps(i)  + pbminps(i)
		c1Result(i) = pbmajps1(i) + pbminps1(i)
		c2Result(i) = pbmajps2(i)  + pbminps2(i)
		c3Result(i) = pbmajps3(i)  + pbminps3(i)
		c4Result(i) = pbmajps4(i)  + pbminps4(i)
	Next i
	eResults = eResult
	c1Results = c1Result
	c2Results = c2Result
	c3Results = c3Result
	c4Results = c4Result

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24009030, 24090301, 24090302, 24090303, 24090304, 0, 0, 0, 0, _
		"Plate Bottom Triaxial Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nPlates, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nPlates, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nPlates, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nPlates, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nPlates, elementIDs, c4Results )
	RBO.Save

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'	Calculate Solid Stress Intensities
'		
'		Output Vector IDs:
'			Solid Stress Intensity - 24060001 (formerly 300050)
'			Solid C1 Stress Intensity - 24600011 (formerly 161)
'			Solid C2 Stress Intensity - 24600012 (formerly 162)
'			Solid C3 Stress Intensity - 24600013 (formerly 163)
'			Solid C4 Stress Intensity - 24600014 (formerly 164)
'			Solid C5 Stress Intensity - 24600015 (formerly 165)
'			Solid C6 Stress Intensity - 24600016 (formerly 166)
'			Solid C7 Stress Intensity - 24600017 (formerly 167)
'			Solid C8 Stress Intensity - 24600018 (formerly 168)
'
'		Stress Intensity is the Maximum Value of one of the three(3) following values:
'			Absolute Value of the Difference between the Maximum and Minimum Principal Stresses
'			Absolute Value of the Difference between the Maximum and Internal Principal Stresses
'			Absolute Value of the Difference between the Minium and Internal Principal Stresses
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculateSolidStressIntensities ( )
'	Get Solid Maximum Principal Stresses
	ovID = 60016
	cID(0) = 70016
	cID(1) = 70516
	cID(2) = 71016
	cID(3) = 71516
	cID(4) = 72016
	cID(5) = 72516
	cID(6) = 73016
	cID(7) = 73516
	cornerIDs = cID
	ovTitle = "Solid Max Prin Stress"
	PopulateRBOWithCornerResults ( )

	For i = 0 To nSolids-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 5, c5Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 6, c6Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 7, c7Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 8, c8Result(i) )
	Next i
	majps = eResult
	majps1 = c1Result
	majps2 = c2Result
	majps3 = c3Result
	majps4 = c4Result
	majps5 = c5Result
	majps6 = c6Result
	majps7 = c7Result
	majps8 = c8Result

'	Get Solid Minumum Principal Stresses
	ovID = 60017
	cID(0) = 70017
	cID(1) = 70517
	cID(2) = 71017
	cID(3) = 71517
	cID(4) = 72017
	cID(5) = 72517
	cID(6) = 73017
	cID(7) = 73517
	cornerIDs = cID
	ovTitle = "Solid Max Prin Stress"

	PopulateRBOWithCornerResults ( )
	For i = 0 To nSolids-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 5, c5Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 6, c6Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 7, c7Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 8, c8Result(i) )
	Next i

	minps = eResult
	minps1 = c1Result
	minps2 = c2Result
	minps3 = c3Result
	minps4 = c4Result
	minps5 = c5Result
	minps6 = c6Result
	minps7 = c7Result
	minps8 = c8Result

'	Get Solid Internal Principal Stresses
	ovID = 60018
	cID(0) = 70018
	cID(1) = 70518
	cID(2) = 71018
	cID(3) = 71518
	cID(4) = 72018
	cID(5) = 72518
	cID(6) = 73018
	cID(7) = 73518
	cornerIDs = cID
	ovTitle = "Solid Int Prin Stress"

	PopulateRBOWithCornerResults ( )
	Dim MaxMinusMin As Double
	Dim MaxMinusInt As Double
	Dim MinMinusInt As Double
	Dim resultant As Double
	ReDim result(nSolids)
	For i = 0 To nSolids-1
		rc = RBO.GetValue ( elementIDs(i), 0, eResult(i) )
		rc = RBO.GetValue ( elementIDs(i), 1, c1Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 2, c2Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 3, c3Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 4, c4Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 5, c5Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 6, c6Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 7, c7Result(i) )
		rc = RBO.GetValue ( elementIDs(i), 8, c8Result(i) )
	Next i

	intps = eResult
	intps1 = c1Result
	intps2 = c2Result
	intps3 = c3Result
	intps4 = c4Result
	intps5 = c5Result
	intps6 = c6Result
	intps7 = c7Result
	intps8 = c8Result

	For i = 0 To nSolids-1
			MaxMinusMin = Abs ( majps(i) - minps(i) )
			MaxMinusInt = Abs ( majps(i) - intps(i) )
			MinMinusInt = Abs ( minps(i) - intps(i) )

			resultant = MaxMinusMin
			If resultant < MaxMinusInt Then
				resultant = MaxMinusInt
			End If
			If resultant < MinMinusInt Then
				resultant = MinMinusInt
			End If
			result(i) = resultant

		For j = 0 To 7
			Select Case j
			Case 0
				max(j) = majps1(i)
				min(j) = minps1(i)
				internal(j) = intps1(j)
			Case 1
				max(j) = majps2(i)
				min(j) = minps2(i)
				internal(j) = intps2(j)
			Case 2
				max(j) = majps3(i)
				min(j) = minps3(i)
				internal(j) = intps3(j)
			Case 3
				max(j) = majps4(i)
				min(j) = minps4(i)
				internal(j) = intps4(j)
			Case 4
				max(j) = majps5(i)
				min(j) = minps5(i)
				internal(j) = intps5(j)
			Case 5
				max(j) = majps6(i)
				min(j) = minps6(i)
				internal(j) = intps6(j)
			Case 6
				max(j) = majps7(i)
				min(j) = minps7(i)
				internal(j) = intps7(j)
			Case 7
				max(j) = majps8(i)
				min(j) = minps8(i)
				internal(j) = intps8(j)
			End Select
			MaxMinusMin = Abs ( max(j) - min(j) )
			MaxMinusInt = Abs ( max(j) - Int(j) )
			MinMinusInt = Abs ( min(j) - Int(j) )

			resultant = MaxMinusMin
			If resultant < MaxMinusInt Then
				resultant = MaxMinusInt
			End If
			If resultant < MinMinusInt Then
				resultant = MinMinusInt
			End If
			corner(j) = resultant
		Next j

		eResult(i) = result(i)
		c1Result(i) = corner(0)
		c2Result(i) = corner(1)
		c3Result(i) = corner(2)
		c4Result(i) = corner(3)
		c5Result(i) = corner(4)
		c6Result(i) = corner(5)
		c7Result(i) = corner(6)
		c8Result(i) = corner(7)

	Next i

	eResults = eResult
	c1Results = c1Result
	c2Results = c2Result
	c3Results = c3Result
	c4Results = c4Result
	c5Results = c5Result
	c6Results = c6Result
	c7Results = c7Result
	c8Results = c8Result

	'Populate the RBO
	RBO.clear
	rc = RBO.SetColumn ( 0, nSolids, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nSolids, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nSolids, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nSolids, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nSolids, elementIDs, c4Results )
	rc = RBO.SetColumn ( 5, nSolids, elementIDs, c5Results )
	rc = RBO.SetColumn ( 6, nSolids, elementIDs, c6Results )
	rc = RBO.SetColumn ( 7, nSolids, elementIDs, c7Results )
	rc = RBO.SetColumn ( 8, nSolids, elementIDs, c8Results )

	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24060001, 24600011, 24600012, 24600013, 24600014, _
		24600015, 24600016, 24600017, 24600018,	"Solid Stress Intensity", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nSolids, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nSolids, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nSolids, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nSolids, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nSolids, elementIDs, c4Results )
	rc = RBO.SetColumn ( 5, nSolids, elementIDs, c5Results )
	rc = RBO.SetColumn ( 6, nSolids, elementIDs, c6Results )
	rc = RBO.SetColumn ( 7, nSolids, elementIDs, c7Results )
	rc = RBO.SetColumn ( 8, nSolids, elementIDs, c8Results )
	RBO.Save

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'	Calculate Solid Triaxial Stresses
'		
'		Output Vector IDs:
'			Solid Triaxial Stress - 24060002 (formerly 7030)
'			Solid C1 Stress Intensity - 24600021 (formerly 171)
'			Solid C2 Stress Intensity - 24600022 (formerly 172)
'			Solid C3 Stress Intensity - 24600023 (formerly 173)
'			Solid C4 Stress Intensity - 24600024 (formerly 174)
'			Solid C5 Stress Intensity - 24600025 (formerly 175)
'			Solid C6 Stress Intensity - 24600026 (formerly 176)
'			Solid C7 Stress Intensity - 24600027 (formerly 177)
'			Solid C8 Stress Intensity - 24600028 (formerly 178)
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculateSolidTriaxialStresses ( )
'	Calculate Solid Triaxial Stresses
	For i = 0 To nSolids-1
		eResult(i) =  majps(i) + minps(i) + intps(i)
		c1Result(i) = majps1(i) + minps1(i) + intps1(i)
		c2Result(i) = majps2(i) + minps2(i) + intps2(i)
		c3Result(i) = majps3(i) + minps3(i) + intps3(i)
		c4Result(i) = majps4(i) + minps4(i) + intps4(i)
		c5Result(i) = majps5(i) + minps5(i) + intps5(i)
		c6Result(i) = majps6(i) + minps6(i) + intps6(i)
		c7Result(i) = majps7(i) + minps7(i) + intps7(i)
		c8Result(i) = majps8(i) + minps8(i) + intps8(i)
	Next i

	eResults = eResult
	c1Results = c1Result
	c2Results = c2Result
	c3Results = c3Result
	c4Results = c4Result
	c5Results = c5Result
	c6Results = c6Result
	c7Results = c7Result
	c8Results = c8Result

	RBO.Clear
	rc = RBO.AddElemWithCornerColumnsV2 (outputSetID, 24060002, 24600021, 24600022, 24600023, 24600024, _
		24600025, 24600026, 24600027, 24600028, "Solid Triaxial Stress", FOT_STRESS, False, newColumns )
	rc = RBO.SetColumn ( 0, nSolids, elementIDs, eResults )
	rc = RBO.SetColumn ( 1, nSolids, elementIDs, c1Results )
	rc = RBO.SetColumn ( 2, nSolids, elementIDs, c2Results )
	rc = RBO.SetColumn ( 3, nSolids, elementIDs, c3Results )
	rc = RBO.SetColumn ( 4, nSolids, elementIDs, c4Results )
	rc = RBO.SetColumn ( 5, nSolids, elementIDs, c5Results )
	rc = RBO.SetColumn ( 6, nSolids, elementIDs, c6Results )
	rc = RBO.SetColumn ( 7, nSolids, elementIDs, c7Results )
	rc = RBO.SetColumn ( 8, nSolids, elementIDs, c8Results )
	RBO.Save

End Sub


'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'
'	Calculate Average Stresses
'
'	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CalculateAverageStress ( )
	For i = 0 To nPlates-1
		c1Result(i) = ( c1TopResults(i) + c1BotResults(i) ) / 2.0
		c2Result(i) = ( c2TopResults(i) + c2BotResults(i) ) / 2.0
		c3Result(i) = ( c3TopResults(i) + c3BotResults(i) ) / 2.0
		c4Result(i) = ( c4TopResults(i) + c4BotResults(i) ) / 2.0
		eResult(i) = ( eTopResults(i) + eBotResults(i) ) / 2.0
	Next i
	c1Results = c1Result
	c2Results = c2Result
	c3Results = c3Result
	c4Results = c4Result
	eResults = eResult
End Sub


Sub SendRBO2DataTable ( )
	App.feDataTable.Lock ( False )
	App.feDataTable.clear
	RBO.SendToDataTable ( )
End Sub


Sub PrintMessage ( )
	App.feAppMessage ( FCM_HIGHLIGHT, msgString )
End Sub