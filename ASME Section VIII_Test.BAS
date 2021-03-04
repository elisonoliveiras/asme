'www.PhDsoft.com
'All Rights Reserved, 2020
'PhDsoft Engineering Assumes No Responsibility For Results Obtained From API
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'API Written by PhDsoft Engineering, Rev-01 (Tested on Femap v2020.2)
'Rev-0 (updated by Andre R. da Rocha, June 2020)

' This API started out as a simple Beam Axial stress calculator.


' For selected output case this program calculates and creates new output vectors:
'	Beams:
'		Beam EndA Axial Stress
'		Beam EndB Axial Stress
'
'	*****
'	Updated September 2019 to replace the use of the feOutput Set and feOutput object
'	for reading and writing results with the feResultsBrowsing object
'
'	This script creates new Output vectors as noted in the
'       in the comments for each subroutine. The Output Vector IDs lie in the range of the new
'       ranges for User Defined Output Vectors from 24,000,000 to 24,999,999
'
'	Subroutines:
'
'		General Use:	PopulateRBOWithCornerResults	(used for Plate and Solid Elements)
'			
'
'		Beam Axial Stresses ==> CalculateBeamStressIntensities
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

'	Element
Dim elem As femap.Elem
Dim elemID As Long
Dim elementID() As Long
Dim elementIDs As Variant

'	Element Property
Dim elemProp As femap.Prop
Dim elemPropID As Long

'	Max, Min, Corner and Variables for Stress Intensity Calculations
Dim max() As Double
Dim min() As Double
Dim internal() As Double
Dim corner() As Double
Dim A As Double
Dim B As Double
Dim cen(2) As Double
Dim diff As Double

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
'	Get Sets of Beam
'	**************************************************
	Set beamElemSet = App.feSet


	Set elem = App.feElem
	Set elemProp = App.feProp

	While elem.Next
		If elem.type = FET_L_BEAM Then
			beamElemSet.Add ( elem.ID )
		End If
	Wend

	nBeams = beamElemSet.Count

	Dim count As Long
	count = outputSets.Count  * ( (nBeams * 2))

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

		outputSetID = outputSets.Next
	Next i

	rc = App.feAppMessage ( 0, "" )
	rc = App.feAppMessage ( FCM_HIGHLIGHT, "Stress Intensity Calculations Complete" )
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
	
	Dim I1() As Double		'Beam Inertia 1	
	Dim I2() As Double		'Beam Inertia 2			
	
	Dim Length() As Double	
	Dim lgt() As Double
	
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
	
	ReDim Length(nBeams)
	ReDim lgt(nBeams)
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
	Dim nColAdd3 As Long
	Dim colIndices3 As Variant	
	Dim newColumns As Variant
	rc = RBO.AddColumnV2 ( OutputSetID, 3022, False, nColAdd1, colIndices1 )
	rc = RBO.AddColumnV2 ( OutputSetID, 3023, False, nColAdd1, colIndices2 )
	
	rc = RBO.AddColumnV2 ( 1, 24000001, False, nColAdd1, colIndices3 )	
	
	RBO.Populate

	For i = 0 To nBeams-1
		elem.Get ( elementIDs(i) )
		elemPropID = elem.propID
		elem.Put ( elementIDs(i) )

		elemProp.Get ( elemPropID )
		area(i) = elemProp.pval(0)
		I1(i) = elemProp.pval(49)		
		elemProp.Put ( elemPropID )

		'Get Values of  the Beam End Axial Forces from the RBO
		RBO.GetValue ( elementIDs(i), colIndices1(0), pA(i) )
		beamAxialStressA(i) = pA(i)/area(i)

		RBO.GetValue ( elementIDs(i), colIndices2(0), pB(i) )
		beamAxialStressB(i) = pB(i)/area(i)
		
		RBO.GetValue ( elementIDs(i), colIndices3(0), Length(i) )
		lgt(i) = Length(i)
		Msg = "For Element " + Str$(elementIDs(i)) + " Length " + Str$(Length(i))
		rc = App.feAppMessage( FCM_NORMAL, Msg )	
	
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

