import "regent"
local configFileName = "default.cfg"

-- Module to handle config class
local su2Config = require("su2_config")

-- Some C APIs
local c = regentlib.c
local cstring = terralib.includec("string.h")
local sqrt = regentlib.sqrt(double)
local cmath = terralib.includec("math.h")
local pow = cmath.pow
local abs = cmath.fabs
local PI = cmath.M_PI

-- Preprocessing step in Lua language
-- File I/O in Lua language comes from the following websites
-- https://stackoverflow.com/questions/11201262/how-to-read-data-from-a-file-in-lua
-- http://lua-users.org/wiki/StringRecipes
-- http://lua-users.org/wiki/StringTrim

-- See if the file exists
function fileExists(file)
	local f = io.open(file,"rb")
	if f then f:close() end
	return f ~= nil
end

-- Trim a string(remove white spaces before and after a string)
function trim(s)
	return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- Check if string X starts with string Y
function stringStarts(String,Start)
	return string.sub(String,1,string.len(Start))==Start
end

-- Read lines until it finds specified string and return its value
function findStringVal(file,stringIn)
	if not fileExists(file) then
		print("'default.cfg' doesn't exist in current directory.")
		print("TERMINATE THE PROGRAM..")
		os.exit()
	end

	local stringVal, startInd, endInd

	for line in io.lines(file) do
		if stringStarts(line,stringIn) then
			startInd, endInd = string.find(line,"=")
			stringVal = trim(string.sub(line,startInd+1,string.len(line)))
			stringVal = tonumber(stringVal)
		end
	end

	return stringVal
end

-- Read lines until it finds meshFileName and return the file name
function findString(file,stringIn)
	if not fileExists(file) then
		print("'default.cfg' doesn't exist in current directory.")
		print("TERMINATE THE PROGRAM..")
		os.exit()
	end

	local stringOut, startInd, endInd

	for line in io.lines(file) do
		if stringStarts(line,stringIn) then
			startInd, endInd = string.find(line,"=")
			stringOut = trim(string.sub(line,startInd+1,string.len(line)))
		end
	end

	return stringOut
end

-- Read number of elements and verticies
function readElemAndPoint(file)
	if not fileExists(file) then
		print("Grid file",file,"doesn't exits in current directory.")
		print("TERMINATE THE PROGRAM..")
		os.exit()
	end

	local K, Nv, line, startInd, endInd
	local f = io.open(file,"rb")
	line = f:read()

	line = trim(line)
	startInd, endInd = string.find(line," ")
	K = tonumber(string.sub(line,1,startInd-1))
	line = string.sub(line,startInd,string.len(line))
	line = trim(line)
	Nv = tonumber(line)
	f:close()

	return K, Nv
end

local p_space	= findStringVal(configFileName,"p_space")
local p_time	= findStringVal(configFileName,"p_time")

local NpG		= (p_space+1)*(p_space+2)/2
local NtG		= p_time+1
local NfpG		= p_space+1

local nSpaceIntG, nTimeIntG
local stdElemSpaceFileName, stdElemTimeFileName
local aderIterMatFileName = "aderIterMat"
if ( p_space == 1 ) then		
	nSpaceIntG = 6;		stdElemSpaceFileName = "standardElementSpace1.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps1"
elseif ( p_space == 2 ) then
	nSpaceIntG = 12;	stdElemSpaceFileName = "standardElementSpace2.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps2"
elseif ( p_space == 3 ) then
	nSpaceIntG = 19;	stdElemSpaceFileName = "standardElementSpace3.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps3"
elseif ( p_space == 4 ) then
	nSpaceIntG = 36;	stdElemSpaceFileName = "standardElementSpace4.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps4"
elseif ( p_space == 5 ) then
	nSpaceIntG = 54;	stdElemSpaceFileName = "standardElementSpace5.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps5"
elseif ( p_space == 6 ) then
	nSpaceIntG = 73;	stdElemSpaceFileName = "standardElementSpace6.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps6"
elseif ( p_space == 7 ) then
	nSpaceIntG = 93;	stdElemSpaceFileName = "standardElementSpace7.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps7"
elseif ( p_space == 8 ) then
	nSpaceIntG = 118;	stdElemSpaceFileName = "standardElementSpace8.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps8"
elseif ( p_space == 9 ) then
	nSpaceIntG = 145;	stdElemSpaceFileName = "standardElementSpace9.dat"
	aderIterMatFileName = aderIterMatFileName .. "Ps9"
else
	print("Only supports p_space = 1, 2, 3, 4, 5, 6, 7, 8, 9.")
	print("Check p_space value in 'default.cfg' file.")
	print("TREMINATE THE PROGRAM..")
	os.exit()
end

if ( p_time == 1 ) then
	nTimeIntG	= 2;	stdElemTimeFileName = "standardElementTime1.dat"
	aderIterMatFileName = aderIterMatFileName .. "Pt1.dat"
elseif ( p_time == 2 ) then
	nTimeIntG	= 3;	stdElemTimeFileName = "standardElementTime2.dat"
	aderIterMatFileName = aderIterMatFileName .. "Pt2.dat"
elseif ( p_time == 3 ) then
	nTimeIntG	= 4;	stdElemTimeFileName = "standardElementTime3.dat"
	aderIterMatFileName = aderIterMatFileName .. "Pt3.dat"
else
	print("Only supports p_time = 1, 2, 3.")
	print("Check p_time value in 'default.cfg' file.")
	print("TREMINATE THE PROGRAM..")
	os.exit()
end

local meshFileName = findString(configFileName,"meshFileName")
gridNvG, gridKG = readElemAndPoint(meshFileName)

local EToEFileName
local EToFFileName
local partFileName
if ( gridKG == 166 ) then
	EToEFileName = "EToE009.dat" ; EToFFileName = "EToF009.dat";
	partFileName = "grid009Part"
elseif ( gridKG == 640 ) then
	EToEFileName = "EToE017.dat" ; EToFFileName = "EToF017.dat";
	partFileName = "grid017Part"
elseif ( gridKG == 2454 ) then
	EToEFileName = "EToE033.dat" ; EToFFileName = "EToF033.dat";
	partFileName = "grid033Part"
elseif ( gridKG == 9638 ) then
	EToEFileName = "EToE065.dat" ; EToFFileName = "EToF065.dat";
	partFileName = "grid065Part"
elseif ( gridKG == 38428 ) then
	EToEFileName = "EToE129.dat" ; EToFFileName = "EToF129.dat";
	partFileName = "grid129Part"
elseif ( gridKG == 163426 ) then
	EToEFileName = "EToE257.dat" ; EToFFileName = "EToF257.dat";
	partFileName = "grid257Part"
elseif ( gridKG == 686300 ) then
	EToEFileName = "EToE513.dat" ; EToFFileName = "EToF513.dat";
	partFileName = "grid513Part"
elseif ( gridKG == 2821764 ) then
	EToEFileName = "EToE1025.dat" ; EToFFileName = "EToF1025.dat";
	partFileName = "grid1025Part"
end

fspace doubleVal {
	v : double
}

fspace uintVal {
	v : uint64
}

fspace GridVertex {
	id				: uint64,					-- Index of vertex
	VX				: double,					-- X coordinate
	VY				: double					-- Y coordinate
}

fspace GridEToV(r : region(GridVertex)) {
	cellColor		: int1d,
	v1				: ptr(GridVertex,r),
	v2				: ptr(GridVertex,r),
	v3				: ptr(GridVertex,r)
}

struct consVar {
	rho				: double;
	rhou			: double;
	rhov			: double;
	ener			: double;
}

fspace Elem {
	sol				: consVar[NpG];				-- Np*K
	volRes			: consVar[NpG];				-- NP*K
	surfRes			: consVar[NpG];				-- NP*K
	preSol			: consVar[NpG*NtG];			-- (Np*Nt)*K
	_x				: double[NpG];				-- Np*K
	_y				: double[NpG];				-- Np*K
	rx				: double[NpG];				-- Np*K
	ry				: double[NpG];				-- Np*K
	sx				: double[NpG];				-- Np*K
	sy				: double[NpG];				-- Np*K
	J				: double[NpG];				-- Np*K
	rxInt			: double[nSpaceIntG];		-- nSpaceInt*K
	ryInt			: double[nSpaceIntG];		-- nSpaceInt*K
	sxInt			: double[nSpaceIntG];		-- nSpaceInt*K
	syInt			: double[nSpaceIntG];		-- nSpaceInt*K
	nx				: double[3*NfpG];			-- (Nfaces*Nfp)*K
	ny				: double[3*NfpG];			-- (Nfaces*Nfp)*K
	sJ				: double[3*NfpG];			-- (Nfaces*Nfp)*K
	Fscale			: double[3*NfpG];			-- (Nfaces*Nfp)*K
	QPInfo			: uint64[3*NfpG][4];		-- 4*(Nfaces*Nfp)
	adjQP			: bool[3];
	e1				: int1d;
	e2				: int1d;
	e3				: int1d;
	cellInd			: uint64;
	cellColor		: int1d;
}

fspace Surface {
	rho			: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	rhou		: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	rhov		: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	ener		: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	faceInd		: uint64;
	faceColor	: int1d;
}

terra skipHeader(f : &c.FILE)
	var xx : uint64, yy : uint64
	c.fscanf(f,"%llu %llu\n",&xx,&yy)
end

terra readCoord(f : &c.FILE, coord : &double)
	var xx : uint64
	return c.fscanf(f,"%d %lf %lf\n",&xx,&coord[0],&coord[1]) == 3
end

terra readEToV(f : &c.FILE, EToVVal : &uint64)
	var xx : uint64, yy : uint64, zz : uint64
	return c.fscanf(f,"%llu %llu %llu %llu %llu %llu\n",&xx,&yy,&zz,&EToVVal[0],&EToVVal[1],&EToVVal[2]) == 6
end

terra isFile(fileName : rawstring)
	var file = c.fopen(fileName,"rb")
	if file == nil then return false end
	c.fclose(file)
	return true
end

terra readDoubleVal(f : &c.FILE, val : &double)
	return c.fscanf(f,"%lf\n",&val[0]) == 1
end

task readStdElemSpaceInfo(fileName : &int8, MSpace : region(ispace(int1d),doubleVal), Dr : region(ispace(int1d),doubleVal), Ds : region(ispace(int1d),doubleVal), Drw : region(ispace(int1d),doubleVal), Dsw : region(ispace(int1d),doubleVal), LIFT : region(ispace(int1d),doubleVal), DrSpaceInt : region(ispace(int1d),doubleVal), DsSpaceInt : region(ispace(int1d),doubleVal), wSpaceInt : region(ispace(int1d),doubleVal), DOFToIntSpaceTranspose : region(ispace(int1d),doubleVal), vmapM : region(ispace(int1d),uintVal) )
where
	reads writes(MSpace.v, Dr.v, Ds.v, Drw.v, Dsw.v, LIFT.v, DrSpaceInt.v, DsSpaceInt.v, wSpaceInt.v, DOFToIntSpaceTranspose.v, vmapM.v)
do
	if isFile(fileName) then
		c.printf("-----------Read Std.Elem Space File---------\n\n")
		var f = c.fopen(fileName,"r")
		var val : double[1]

		-- MSpace
		for e in MSpace do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			MSpace[e].v = val[0]
		end

		-- Dr
		for e in Dr do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			Dr[e].v = val[0]
		end

		-- Ds
		for e in Ds do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			Ds[e].v = val[0]
		end

		-- Drw
		for e in Drw do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			Drw[e].v = val[0]
		end

		-- Dsw
		for e in Dsw do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			Dsw[e].v = val[0]
		end

		-- LIFT
		for e in LIFT do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			LIFT[e].v = val[0]
		end

		-- DrSpaceInt
		for e in DrSpaceInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			DrSpaceInt[e].v = val[0]
		end

		-- DsSpaceInt
		for e in DsSpaceInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			DsSpaceInt[e].v = val[0]
		end

		-- wSpaceInt
		for e in wSpaceInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			wSpaceInt[e].v = val[0]
		end

		-- DOFToIntSpaceTranspose
		for e in DOFToIntSpaceTranspose do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			DOFToIntSpaceTranspose[e].v = val[0]
		end
		c.fclose(f)

		-- vmapM
		var jj : int
		for e in vmapM do
			jj = [int](e)
			if ( jj > NfpG*2-1) then
				if ( jj == NfpG*2 ) then
					vmapM[e].v = 0
				else
					vmapM[e].v = vmapM[e-1].v + (NfpG+1- (jj-NfpG*2))
				end
			elseif ( jj > NfpG*1-1 ) then
				if ( jj == NfpG ) then
					vmapM[e].v = vmapM[e-1].v
				else 
					vmapM[e].v = vmapM[e-1].v +  (NfpG - (jj-NfpG))
				end
			else
				vmapM[e].v = jj
			end
		end
	else
		c.printf("File '%s' doesn't exists! Abort the program.\n",fileName)
		c.abort()
	end
end

task readStdElemTimeInfo(fileName : &int8, lFirst : region(ispace(int1d),doubleVal), wTimeInt : region(ispace(int1d),doubleVal), DOFToIntTime : region(ispace(int1d),doubleVal))
where
	reads writes(lFirst.v, wTimeInt.v, DOFToIntTime.v) 
do
	if isFile(fileName) then
		c.printf("-----------Read Std.Elem Time File----------\n\n")
		var f = c.fopen(fileName,"r")
		var val : double[1]

		-- lFirst
		for e in lFirst do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			lFirst[e].v = val[0]
		end

		-- wTimeInt
		for e in wTimeInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			wTimeInt[e].v = val[0]
		end

		-- DOFToIntTime
		for e in DOFToIntTime do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			DOFToIntTime[e].v = val[0]
		end
	else
		c.printf("File '%s' doesn't exists! Abort the program.\n",fileName)
		c.abort()
	end
end

task readAderIterMatInfo(fileName : &int8, AderIterMat : region(ispace(int1d),doubleVal))
where
	reads writes(AderIterMat.v)
do
	if isFile(fileName) then
		c.printf("------------Read aderIterMat File-----------\n\n")
		var f = c.fopen(fileName,"r")
		var val : double[1]

		-- AderIterMat
		for e in AderIterMat do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementInfo file")
			AderIterMat[e].v = val[0]
		end
		c.fclose(f)
	else
		c.printf("File '%s' doesn't exists! Abort the program.\n",fileName)
		c.abort()
	end
end

task readVertex(meshFileName : &int8, gridVertex : region(ispace(ptr),GridVertex))
where
	reads writes(gridVertex)
do
	var coord       : double[2]
	var lowerBound  : uint64

	for e in gridVertex do
		lowerBound = [uint64](e)
		break
	end
	var f = c.fopen(meshFileName,"r")
	skipHeader(f)

	-- Reads data until corresponding partition
	for ii=0, lowerBound do
		regentlib.assert(readCoord(f,coord),"Error in readMesh task. Less coordinates data than it should be. Check grid file.")
	end

	-- Read coordinates
	for e in gridVertex do
		regentlib.assert(readCoord(f,coord),"Error in readMesh task. Less coordinates data than it should be. Check grid file.")
		e.id = [uint64](e)
		e.VX = coord[0]
		e.VY = coord[1]
	end
	c.fclose(f)
end

task readElemToVertex(meshFileName : &int8, gridNv : uint64, gridVertex : region(GridVertex), gridEToV : region(GridEToV(gridVertex)))
where
	reads writes(gridEToV)
do
	var coord       : double[2]
	var EToVVal     : uint64[3]
	var lowerBound  : uint64

	for e in gridEToV do
		lowerBound = [uint64](e)
		break
	end

	var f = c.fopen(meshFileName,"r")
	skipHeader(f)
	
	-- Reads data until EToV info
	for ii=0, gridNv do
		regentlib.assert(readCoord(f,coord),"Error in readMesh task. Less coordinates data than it should be. Check grid file.")
	end

	-- Reads data until corresponding partition
	for ii=0, lowerBound do
		regentlib.assert(readEToV(f,EToVVal),"Error in readMesh task. Less EToV data than it should be. Check grid file.")
	end

	-- Read EToV info
	for e in gridEToV do
		regentlib.assert(readEToV(f,EToVVal),"Error in readMesh task. Less EToV data than it should be. Check grid file.")
		var v1 = unsafe_cast(ptr(GridVertex, gridVertex), EToVVal[0]-1)
		var v2 = unsafe_cast(ptr(GridVertex, gridVertex), EToVVal[1]-1)
		var v3 = unsafe_cast(ptr(GridVertex, gridVertex), EToVVal[2]-1)
		e.v1 = v1
		e.v2 = v2
		e.v3 = v3
	end
	c.fclose(f)
end

terra readData(f : &c.FILE, EVal : &uint64)
	return c.fscanf(f, "%llu %llu %llu\n", &EVal[0],&EVal[1],&EVal[2]) == 3
end

task generateQPConnectivity(EToEFileName : &int8, EToFFileName : &int8, Nfp : uint64, q : region(ispace(ptr),Elem)) 
where
	reads writes(q.cellInd, q.QPInfo, q.adjQP, q.e1, q.e2, q.e3) 
do
	var EToEVal : uint64[3]
	var	EToFVal : uint64[3]
	var dummyVal: uint64
	var faceBase: int32
	var faceGrad: int32
	var lowerBound  : uint64

	for e in q do
		lowerBound = [uint64](e)
		break
	end

	var ff = c.fopen(EToEFileName,"r")
	var gg = c.fopen(EToFFileName,"r")
	for ii=0,lowerBound do
		regentlib.assert(readData(ff,EToEVal), "Error in generateQPConnectivity task. Check EToE data or number of elements value.")
		regentlib.assert(readData(gg,EToFVal), "Error in generateQPConnectivity task. Check EToF data or number of elements value.")
	end

	for e in q do
		e.cellInd = lowerBound
		e.adjQP[0] = true
		e.adjQP[1] = true
		e.adjQP[2] = true

		-- Read EToE and EToF data
		regentlib.assert(readData(ff,EToEVal), "Error in generateQPConnectivity task. Check EToE data or number of elements value.")
		regentlib.assert(readData(gg,EToFVal), "Error in generateQPConnectivity task. Check EToF data or number of elements value.")

		-- Generate QPConnectivity values
		for jj=0,3 do
			if ( EToFVal[jj] > 9 ) then
				faceBase = Nfp*EToFVal[jj]/10-1
				faceGrad = -1
			else
				faceBase = Nfp*(EToFVal[jj]-1)
				faceGrad = 1
			end

			if ( jj == 0 ) then
				for kk=0,Nfp do
					e.QPInfo[0][jj*Nfp+kk] = kk -- Current DOF
					e.QPInfo[1][jj*Nfp+kk] = e.cellInd -- Current cell
					e.QPInfo[2][jj*Nfp+kk] = [uint64](faceBase + faceGrad*kk) -- Target DOF
					e.QPInfo[3][jj*Nfp+kk] = EToEVal[jj] -- Target cell
					e.e1 = [int1d](EToEVal[jj])
				end
			elseif ( jj == 1 ) then
				dummyVal = Nfp-1
				for kk=0,Nfp do
					e.QPInfo[0][jj*Nfp+kk] = dummyVal -- Current DOF
					e.QPInfo[1][jj*Nfp+kk] = e.cellInd -- Current cell
					e.QPInfo[2][jj*Nfp+kk] = [uint64](faceBase + faceGrad*kk) -- Target DOF
					e.QPInfo[3][jj*Nfp+kk] = EToEVal[jj] -- Target cell
					e.e2 = [int1d](EToEVal[jj])
					dummyVal = dummyVal + (Nfp-1-kk)
				end
			elseif ( jj == 2 ) then
				dummyVal = 0
				for kk=0,Nfp do
					e.QPInfo[0][jj*Nfp+kk] = dummyVal -- Current DOF
					e.QPInfo[1][jj*Nfp+kk] = e.cellInd -- Current cell
					e.QPInfo[2][jj*Nfp+kk] = [uint64](faceBase + faceGrad*kk) -- Target DOF
					e.QPInfo[3][jj*Nfp+kk] = EToEVal[jj] -- Target cell
					e.e3 = [int1d](EToEVal[jj])
					dummyVal = dummyVal + (Nfp-kk)
				end
			end
		end
		lowerBound = lowerBound + 1
	end
	c.fclose(ff)
	c.fclose(gg)
end

terra readColor(f : &c.FILE, colorInfo : &uint64)
	return c.fscanf(f,"%llu %llu\n",&colorInfo[0],&colorInfo[1]) == 2
end

task colorElem(partFileName : &int8, parallelism : uint64, gridVertex : region(GridVertex), gridEToV : region(GridEToV(gridVertex)), q : region(ispace(ptr),Elem))
where
	reads writes(q.cellColor, gridEToV.cellColor)
do
	var colorInfo : uint64[2]
	var lowerBound  : uint64
	for e in q do
		lowerBound = [uint64](e)
		break
	end

	if ( parallelism == 1 ) then
		for e in q do
			e.cellColor = 0
		end
	else
		var f = c.fopen(partFileName,"r")
		for ii=0,lowerBound do
			regentlib.assert(readColor(f,colorInfo),"Error in partitioning info. Check partitioning data file.")
		end
		for e in q do
			regentlib.assert(readColor(f,colorInfo),"Error in partitioning info. Check partitioning data file.")
			e.cellColor = [int1d](colorInfo[1])
		end
		c.fclose(f)

		f = c.fopen(partFileName,"r")
		for ii=0,lowerBound do
			regentlib.assert(readColor(f,colorInfo),"Error in partitioning info. Check partitioning data file.")
		end
		for e in gridEToV do
			regentlib.assert(readColor(f,colorInfo),"Error in partitioning info. Check partitioning data file.")
			e.cellColor = [int1d](colorInfo[1])
		end
		c.fclose(f)
	end
end

task colorFaces(Nfp : uint64, q : region(ispace(ptr), Elem), qEqual : region(ispace(ptr), Elem), QMFace : region(ispace(ptr), Surface), QPFace : region(ispace(ptr), Surface))
where
	reads(q.cellColor, qEqual.cellColor, qEqual.adjQP, qEqual.QPInfo, QMFace.faceInd, QMFace.faceColor, QPFace.faceInd, QPFace.faceColor),
	writes(qEqual.adjQP, QMFace.faceInd, QMFace.faceColor, QPFace.faceInd, QPFace.faceColor)
do
	var cellInd	: uint64
	var colorVal	: uint64
	var lowerBound	: uint64

	for e in qEqual do
		lowerBound = [uint64](e)
		break
	end

	for e in qEqual do
		colorVal = e.cellColor
		QMFace[lowerBound].faceColor= colorVal
		QMFace[lowerBound].faceInd	= lowerBound
		QPFace[lowerBound].faceColor= colorVal
		QPFace[lowerBound].faceInd	= lowerBound
		for jj=0,3 do
			cellInd = e.QPInfo[3][jj*Nfp]
			if not ( colorVal == [uint64](q[cellInd].cellColor) ) then
				-- Adjacent cell is not in the same partition
				e.adjQP[jj] = false
			end
		end
		lowerBound = lowerBound + 1
	end
end

task buildNodes(p_space : int8, gridVertex : region(GridVertex), gridEToV : region(GridEToV(gridVertex)), q : region(ispace(ptr), Elem))
where
	reads(q._x,q._y,q.cellInd,gridVertex.VX,gridVertex.VY,gridEToV.v1,gridEToV.v2, gridEToV.v3),
	writes(q._x,q._y)
do
	var dh			: double = 2.0/p_space
	var upBound		: uint64
	var rDum		: double
	var sDum		: double

	var cellNum : uint64
	var cnt : uint64
	for e in q do
		cellNum = q[e].cellInd
		cnt = 0
		for ii=0,(p_space+1) do
			sDum = -1.0 + ii*dh
			upBound = p_space - ii
			for jj=0,upBound+1 do
				rDum = -1.0 + jj*dh
				q[cellNum]._x[cnt] = 0.5*(-(rDum+sDum)*gridVertex[gridEToV[cellNum].v1].VX
									+ (1.0+rDum)*gridVertex[gridEToV[cellNum].v2].VX
									+ (1.0+sDum)*gridVertex[gridEToV[cellNum].v3].VX )
				q[cellNum]._y[cnt] = 0.5*(-(rDum+sDum)*gridVertex[gridEToV[cellNum].v1].VY
									+ (1.0+rDum)*gridVertex[gridEToV[cellNum].v2].VY
									+ (1.0+sDum)*gridVertex[gridEToV[cellNum].v3].VY )
				cnt = cnt + 1
			end
		end
	end
end

task calcGeoFacAndNormal(p_space : int8, nSpaceInt : int8, Dr : region(ispace(int1d), doubleVal), Ds : region(ispace(int1d), doubleVal), DrSpaceInt : region(ispace(int1d), doubleVal), DsSpaceInt : region(ispace(int1d), doubleVal), q : region(ispace(ptr), Elem))
where
	reads(q._x, q._y, q.rx, q.sx, q.ry, q.sy, q.J, q.rxInt, q.sxInt, q.ryInt, q.syInt, q.nx, q.ny, q.sJ, q.Fscale, q.cellInd, Dr.v, Ds.v, DrSpaceInt.v, DsSpaceInt.v),
	writes(q.rx, q.sx, q.ry, q.sy, q.J, q.rxInt, q.sxInt, q.ryInt, q.syInt, q.nx, q.ny, q.sJ, q.Fscale)
do
	var Nfp		: uint64 = p_space + 1
	var NpLoc	: uint64 = (p_space+1)*(p_space+2)/2
	var curDOF	: uint64
	var cnt		: uint64
	var xr		: double
	var xs		: double
	var yr		: double
	var ys		: double
	var JVal	: double

	var cellNum : uint64
	for e in q do
		cellNum = q[e].cellInd
	
		-- J, rx, sx, ry, sy
		for jj=0,NpLoc do
			xr = 0.0
			xs = 0.0
			yr = 0.0
			ys = 0.0
			for kk=0,NpLoc do	
				xr += Dr[jj*NpLoc+kk].v*q[cellNum]._x[kk]
				xs += Ds[jj*NpLoc+kk].v*q[cellNum]._x[kk]
				yr += Dr[jj*NpLoc+kk].v*q[cellNum]._y[kk]
				ys += Ds[jj*NpLoc+kk].v*q[cellNum]._y[kk]
			end
			JVal = ( -xs*yr + xr*ys )
			q[cellNum].J[jj] = JVal
            q[cellNum].rx[jj] =  ys/JVal
            q[cellNum].sx[jj] = -yr/JVal
            q[cellNum].ry[jj] = -xs/JVal
            q[cellNum].sy[jj] =  xr/JVal
		end

		-- rxInt, sxInt, ryInt, syInt
		for jj=0, nSpaceInt do
			xr = 0.0
			xs = 0.0
			yr = 0.0
			ys = 0.0
			for kk=0,NpLoc do
				xr += DrSpaceInt[jj*NpLoc+kk].v*q[cellNum]._x[kk]
                xs += DsSpaceInt[jj*NpLoc+kk].v*q[cellNum]._x[kk]
                yr += DrSpaceInt[jj*NpLoc+kk].v*q[cellNum]._y[kk]
                ys += DsSpaceInt[jj*NpLoc+kk].v*q[cellNum]._y[kk]
			end
			JVal = ( -xs*yr + xr*ys )
			q[cellNum].rxInt[jj] =  ys/JVal
			q[cellNum].sxInt[jj] = -yr/JVal
			q[cellNum].ryInt[jj] = -xs/JVal
			q[cellNum].syInt[jj] =  xr/JVal
		end

		-- Normal Vector
		cnt = 0

		-- Face 1
		for jj=0, Nfp do
			curDOF = jj
			xr = 0.0
			xs = 0.0
			yr = 0.0
			ys = 0.0
			for kk=0,NpLoc do
				xr += Dr[curDOF*NpLoc+kk].v*q[cellNum]._x[kk]
				xs += Ds[curDOF*NpLoc+kk].v*q[cellNum]._x[kk]
				yr += Dr[curDOF*NpLoc+kk].v*q[cellNum]._y[kk]
				ys += Ds[curDOF*NpLoc+kk].v*q[cellNum]._y[kk]
			end
			q[cellNum].nx[cnt] =  yr
			q[cellNum].ny[cnt] = -xr

			-- Normalize the normal vectors and calculate Fscale
			q[cellNum].sJ[cnt] = sqrt( q[cellNum].nx[cnt]*q[cellNum].nx[cnt] + q[cellNum].ny[cnt]*q[cellNum].ny[cnt] )
			q[cellNum].nx[cnt] = q[cellNum].nx[cnt]/q[cellNum].sJ[cnt]
			q[cellNum].ny[cnt] = q[cellNum].ny[cnt]/q[cellNum].sJ[cnt]
			q[cellNum].Fscale[cnt] = q[cellNum].sJ[cnt]/q[cellNum].J[curDOF]
			cnt = cnt + 1
		end

		-- Face 2
		curDOF = Nfp-1
		for jj=0, Nfp do
			xr = 0.0
			xs = 0.0
			yr = 0.0
			ys = 0.0
			for kk=0,NpLoc do
				xr += Dr[curDOF*NpLoc+kk].v*q[cellNum]._x[kk]
				xs += Ds[curDOF*NpLoc+kk].v*q[cellNum]._x[kk]
				yr += Dr[curDOF*NpLoc+kk].v*q[cellNum]._y[kk]
				ys += Ds[curDOF*NpLoc+kk].v*q[cellNum]._y[kk]
			end
			q[cellNum].nx[cnt] =  ys-yr
			q[cellNum].ny[cnt] = -xs+xr

			-- Normalize the normal vectors and calculate Fscale
			q[cellNum].sJ[cnt] = sqrt( q[cellNum].nx[cnt]*q[cellNum].nx[cnt] + q[cellNum].ny[cnt]*q[cellNum].ny[cnt] )
			q[cellNum].nx[cnt] = q[cellNum].nx[cnt]/q[cellNum].sJ[cnt]
			q[cellNum].ny[cnt] = q[cellNum].ny[cnt]/q[cellNum].sJ[cnt]
			q[cellNum].Fscale[cnt] = q[cellNum].sJ[cnt]/q[cellNum].J[curDOF]
			curDOF = curDOF + ((Nfp-1)-jj)
			cnt = cnt + 1
		end

		-- Face 3
		curDOF = 0
		for jj=0, Nfp do
			xr = 0.0
			xs = 0.0
			yr = 0.0
			ys = 0.0
			for kk=0,NpLoc do
				xr += Dr[curDOF*NpLoc+kk].v*q[cellNum]._x[kk]
				xs += Ds[curDOF*NpLoc+kk].v*q[cellNum]._x[kk]
				yr += Dr[curDOF*NpLoc+kk].v*q[cellNum]._y[kk]
				ys += Ds[curDOF*NpLoc+kk].v*q[cellNum]._y[kk]
			end
			q[cellNum].nx[cnt] = -ys
			q[cellNum].ny[cnt] =  xs

			-- Normalize the normal vectors and calculate Fscale
			q[cellNum].sJ[cnt] = sqrt( q[cellNum].nx[cnt]*q[cellNum].nx[cnt] + q[cellNum].ny[cnt]*q[cellNum].ny[cnt] )
			q[cellNum].nx[cnt] = q[cellNum].nx[cnt]/q[cellNum].sJ[cnt]
			q[cellNum].ny[cnt] = q[cellNum].ny[cnt]/q[cellNum].sJ[cnt]
			q[cellNum].Fscale[cnt] = q[cellNum].sJ[cnt]/q[cellNum].J[curDOF]
			curDOF = curDOF + (Nfp-jj)
			cnt = cnt + 1
		end
	end
end

task solutionAtTimeT(time : double, Np : uint64, epsVal : double, rho0Val : double, u0Val : double, v0Val : double, p0Val : double, q : region(ispace(ptr), Elem))
where
	reads (q._x, q._y, q.sol, q.cellInd),
	writes(q.sol)
do
	var gamma	: double = 1.4
	var epsilon	: double = epsVal
	var rho0	: double = rho0Val
	var u0		: double = u0Val
	var v0		: double = v0Val
	var p0		: double = p0Val
	var xDis	: double
	var yDis	: double
	var r		: double
	var rho1	: double
	var u1		: double
	var v1		: double
	var p1		: double
	var cellNum : uint64

	-- Calculate vortex center at current time
	var xCenter : double = 5.0 + u0*time
	var yCenter : double = 0.0 + v0*time
	var cnt		: uint64
	if ( xCenter > 10.0 ) then
		cnt = xCenter/10
		for ii=0,cnt do
			xCenter -= 10.0
			yCenter -= 10.0
		end
	end

	for e in q do
		cellNum = q[e].cellInd

		for jj=0,Np do
			xDis = q[cellNum]._x[jj] - xCenter
			yDis = q[cellNum]._y[jj] - yCenter
			r = sqrt( xDis*xDis + yDis*yDis)

			u1 = u0 - epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*yDis
			v1 = v0 + epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*xDis
			rho1 = pow(( 1.0 - (gamma-1.0)*epsilon*epsilon/(8.0*gamma*PI*PI)*cmath.exp(1.0-r*r)),(1.0/(gamma-1.0)))
			p1 = pow(rho1,gamma)

			q[cellNum].sol[jj].rho = rho1
			q[cellNum].sol[jj].rhou = rho1*u1
			q[cellNum].sol[jj].rhov = rho1*v1
			q[cellNum].sol[jj].ener = p1/(gamma-1.0) + 0.5*rho1*(u1*u1+v1*v1)
		end
	end
end

task Euler2DDT(Nfp : int8, p_space : int8, CFL : double, q : region(ispace(ptr), Elem))
where
	reads(q.sol, q.Fscale, q.cellInd)
do
	var u		: double
	var v		: double
	var p		: double
	var a		: double
	var gamma	: double = 1.4
	var dt		: double = 1.0e+308
	var dtOut	: double = 1.0e+308
	var cellNum	: uint64
	var curDOF	: uint64
	var cnt		: uint64

    for e in q do
        cellNum = q[e].cellInd

		-- Face 1
		cnt = 0
		for jj=0, Nfp do
			curDOF = jj
			u = q[cellNum].sol[curDOF].rhou/q[cellNum].sol[curDOF].rho
			v = q[cellNum].sol[curDOF].rhov/q[cellNum].sol[curDOF].rho
			p = (gamma-1.0)*( q[cellNum].sol[curDOF].ener - q[cellNum].sol[curDOF].rho*(u*u + v*v)/2.0 )
			a = sqrt(abs(gamma*p/q[cellNum].sol[curDOF].rho))

			dt = 1.0/( pow((p_space+1.0),2.0)*0.5*q[cellNum].Fscale[cnt]*(sqrt(u*u + v*v) + a) )
			if dt <= dtOut then
				dtOut = dt
			end
			cnt = cnt + 1
		end

		-- Face 2
		curDOF = Nfp-1
		for jj=0, Nfp do
			u = q[cellNum].sol[curDOF].rhou/q[cellNum].sol[curDOF].rho
			v = q[cellNum].sol[curDOF].rhov/q[cellNum].sol[curDOF].rho
			p = (gamma-1.0)*( q[cellNum].sol[curDOF].ener - q[cellNum].sol[curDOF].rho*(u*u + v*v)/2.0 )
			a = sqrt(abs(gamma*p/q[cellNum].sol[curDOF].rho))

			dt = 1.0/( pow((p_space+1.0),2.0)*0.5*q[cellNum].Fscale[cnt]*(sqrt(u*u + v*v) + a) )
			if dt <= dtOut then
				dtOut = dt
			end
			curDOF = curDOF + ((Nfp-1)-jj)
			cnt = cnt + 1
		end

		-- Face 3
		curDOF = 0
		for jj=0, Nfp do
			u = q[cellNum].sol[curDOF].rhou/q[cellNum].sol[curDOF].rho
			v = q[cellNum].sol[curDOF].rhov/q[cellNum].sol[curDOF].rho
			p = (gamma-1.0)*( q[cellNum].sol[curDOF].ener - q[cellNum].sol[curDOF].rho*(u*u + v*v)/2.0 )
			a = sqrt(abs(gamma*p/q[cellNum].sol[curDOF].rho))

			dt = 1.0/( pow((p_space+1.0),2.0)*0.5*q[cellNum].Fscale[cnt]*(sqrt(u*u + v*v) + a) )
			if dt <= dtOut then
				dtOut = dt
			end
			curDOF = curDOF + (Nfp-jj)
			cnt = cnt + 1
		end
	end

	return	dtOut*CFL
end

task calcTolSolAderRho(Np : uint64, q : region(ispace(ptr), Elem))
where
	reads(q.sol, q.cellInd)
do
	var thres	: double = 1.0e-6
	var maxRho	: double = 1.0e-6
	var cellNum : uint64

	for e in q do
		cellNum = q[e].cellInd
		for jj=0,Np do
			maxRho  = max(maxRho,q[cellNum].sol[jj].rho*thres)
		end
	end

	return maxRho
end

task calcTolSolAderRhouRhov(Np : uint64, q : region(ispace(ptr), Elem))
where
	reads(q.sol, q.cellInd)
do
	var thres	: double = 1.0e-6
	var maxRhou : double = 1.0e-6
	var maxRhov : double = 1.0e-6
	var cellNum : uint64

	for e in q do
		cellNum = q[e].cellInd
		for jj=0,Np do
            maxRhou = max(maxRhou,q[cellNum].sol[jj].rhou*thres)
            maxRhov = max(maxRhov,q[cellNum].sol[jj].rhov*thres)
		end
	end
	maxRhou = max(maxRhou,maxRhov)

	return maxRhou
end

task calcTolSolAderEner(Np : uint64, q : region(ispace(ptr), Elem))
where
	reads(q.sol, q.cellInd)
do
	var thres	: double = 1.0e-6
	var maxEner : double = 1.0e-6
	var cellNum : uint64

	for e in q do
		cellNum = q[e].cellInd
		for jj=0,Np do
            maxEner = max(maxEner,q[cellNum].sol[jj].ener*thres)
		end
	end

	return maxEner
end

__demand(__inline)
task Euler2DPredictor(cellNum : uint64, Np : uint64, Nt : uint64, nSpaceInt : uint64, nTimeInt : uint64, dt : double, tolSolAderRho : double, tolSolAderRhouRhov : double, tolSolAderEner : double, MSpace : region(ispace(int1d), doubleVal), DrSpaceInt : region(ispace(int1d), doubleVal), DsSpaceInt : region(ispace(int1d), doubleVal), wSpaceInt : region(ispace(int1d), doubleVal), DOFToIntSpaceTranspose : region(ispace(int1d), doubleVal), lFirst : region(ispace(int1d), doubleVal), wTimeInt : region(ispace(int1d), doubleVal), DOFToIntTime : region(ispace(int1d), doubleVal), AderIterMat : region(ispace(int1d),doubleVal), q : region(ispace(ptr), Elem), preSolRho : &double, preSolRhou : &double, preSolRhov : &double, preSolEner : &double)
where
    reads(MSpace.v, DrSpaceInt.v, DsSpaceInt.v, wSpaceInt.v, DOFToIntSpaceTranspose.v, lFirst.v, wTimeInt.v, DOFToIntTime.v, AderIterMat.v, q.sol, q.rxInt, q.sxInt, q.ryInt, q.syInt)
do
	var cntTot 			: uint64 = 0
	var indVal			: uint64
	var p				: double
	var u				: double
	var v				: double
	var rH				: double
	var gamma			: double = 1.4
	var w				: double
	var fluxF1			: double[55]		-- Np, Ps=9 
	var fluxF2			: double[55]		-- Np
	var fluxF3			: double[55]		-- Np
	var fluxF4			: double[55]		-- Np
	var fluxG1			: double[55]		-- Np
	var fluxG2			: double[55]		-- Np
	var fluxG3			: double[55]		-- Np
	var fluxG4			: double[55]		-- Np
	var gradFluxesIntFr1: double[145]		-- nSpaceInt, Ps=9 
	var gradFluxesIntFr2: double[145]		-- nSpaceInt
	var gradFluxesIntFr3: double[145]		-- nSpaceInt
	var gradFluxesIntFr4: double[145]		-- nSpaceInt
	var gradFluxesIntFs1: double[145]		-- nSpaceInt
	var gradFluxesIntFs2: double[145]		-- nSpaceInt
	var gradFluxesIntFs3: double[145]		-- nSpaceInt
	var gradFluxesIntFs4: double[145]		-- nSpaceInt
	var gradFluxesIntGr1: double[145]		-- nSpaceInt
	var gradFluxesIntGr2: double[145]		-- nSpaceInt
	var gradFluxesIntGr3: double[145]		-- nSpaceInt
	var gradFluxesIntGr4: double[145]		-- nSpaceInt
	var gradFluxesIntGs1: double[145]		-- nSpaceInt
	var gradFluxesIntGs2: double[145]		-- nSpaceInt
	var gradFluxesIntGs3: double[145]		-- nSpaceInt
	var gradFluxesIntGs4: double[145]		-- nSpaceInt
	var resSolRho		: double[220]		-- (Np*Nt), Ps=9, Pt=3
	var resSolRhou		: double[220]		-- (Np*Nt)
	var resSolRhov		: double[220]		-- (Np*Nt)
	var resSolEner		: double[220]		-- (Np*Nt)
	var oldSolRho		: double[220]		-- (Np*Nt)
	var oldSolRhou		: double[220]		-- (Np*Nt)
	var oldSolRhov		: double[220]		-- (Np*Nt)
	var oldSolEner		: double[220]		-- (Np*Nt)
	var intSolRho		: double[55]		-- Np
	var intSolRhou		: double[55]		-- Np
	var intSolRhov		: double[55]		-- Np
	var intSolEner		: double[55]		-- Np
	var resIntRho		: double[55]		-- Np
	var resIntRhou		: double[55]		-- Np
	var resIntRhov		: double[55]		-- Np
	var resIntEner		: double[55]		-- Np
	var resDumRho		: double[145]		-- nSpaceInt 
	var resDumRhou		: double[145]		-- nSpaceInt 
	var resDumRhov		: double[145]		-- nSpaceInt 
	var resDumEner		: double[145]		-- nSpaceInt 
	var resTotRho		: double[220]		-- (Np*Nt)
	var resTotRhou		: double[220]		-- (Np*Nt)
	var resTotRhov		: double[220]		-- (Np*Nt)
	var resTotEner		: double[220]		-- (Np*Nt)
	var isConverged		: bool = true
	var NtNp			: uint64 = Nt*Np

    -- Initialize the predictor solution by current solution
	indVal = 0
	for jj=0,Nt do
		for kk=0,Np do
			preSolRho[indVal]	= q[cellNum].sol[kk].rho
			preSolRhou[indVal]	= q[cellNum].sol[kk].rhou
			preSolRhov[indVal]	= q[cellNum].sol[kk].rhov
			preSolEner[indVal]	= q[cellNum].sol[kk].ener 
			indVal = indVal + 1
		end
	end

    -- Compute residual contribution of current solution
	for jj=0,Np do
		resSolRho[jj]	= 0.0
		resSolRhou[jj]	= 0.0
		resSolRhov[jj]	= 0.0
		resSolEner[jj]	= 0.0
		indVal = 0
		for kk=0,Np do
			resSolRho[jj]	+= MSpace[jj*Np+kk].v*preSolRho[indVal]
			resSolRhou[jj]	+= MSpace[jj*Np+kk].v*preSolRhou[indVal]
			resSolRhov[jj]	+= MSpace[jj*Np+kk].v*preSolRhov[indVal]
			resSolEner[jj]	+= MSpace[jj*Np+kk].v*preSolEner[indVal]
			indVal = indVal + 1
		end
	end

    for jj=1,Nt do
		for kk=0,Np do
            resSolRho[jj*Np+kk]	= resSolRho[kk] *lFirst[jj].v
            resSolRhou[jj*Np+kk]= resSolRhou[kk]*lFirst[jj].v
            resSolRhov[jj*Np+kk]= resSolRhov[kk]*lFirst[jj].v
            resSolEner[jj*Np+kk]= resSolEner[kk]*lFirst[jj].v
        end
    end

    for jj=0,Np do
        resSolRho[jj]    *= lFirst[0].v
        resSolRhou[jj]	 *= lFirst[0].v
        resSolRhov[jj]	 *= lFirst[0].v
        resSolEner[jj]	 *= lFirst[0].v
    end

	-- Iterative algorithm to compute the predictor solution
	cntTot = 0
	for iii=0,100 do
		-- Initialize the total residual with resSol and
		-- store the current solution in oldSol
		indVal = 0
		for jj=0,Np*Nt do
			resTotRho[jj]   = resSolRho[jj]
			resTotRhou[jj]	= resSolRhou[jj]
			resTotRhov[jj]	= resSolRhov[jj]
			resTotEner[jj]	= resSolEner[jj]
			oldSolRho[jj]	= preSolRho[indVal]
			oldSolRhou[jj]	= preSolRhou[indVal] 
			oldSolRhov[jj]	= preSolRhov[indVal] 
			oldSolEner[jj]	= preSolEner[indVal]
			indVal = indVal + 1
		end

		-- Loop over the number of integration points in time
		-- to compute the space-time integral of the spatial
		-- derivatives of the fluxes
		for jj=0,nTimeInt do
			-- Intitialize the container to zero
			for kk=0,Np do
				intSolRho[kk]   = 0.0
				intSolRhou[kk]	= 0.0
				intSolRhov[kk]	= 0.0
				intSolEner[kk]	= 0.0
			end

			-- Interpolate the predictor solution to the
			-- current temporal integration point
			indVal = 0
			for kk=0,Nt do
				for ll=0,Np do
					intSolRho[ll]	+= DOFToIntTime[jj*Nt+kk].v*preSolRho[indVal]
					intSolRhou[ll]	+= DOFToIntTime[jj*Nt+kk].v*preSolRhou[indVal] 
					intSolRhov[ll]	+= DOFToIntTime[jj*Nt+kk].v*preSolRhov[indVal] 
					intSolEner[ll]	+= DOFToIntTime[jj*Nt+kk].v*preSolEner[indVal]
					indVal = indVal + 1
				end
			end

			--!! ADER_DG_AliasedPredictorResidual
			-- Calculate Euler fluxes in 2D
			for kk=0,Np do
				-- Calculate primitive variables
				u = intSolRhou[kk]/intSolRho[kk]
				v = intSolRhov[kk]/intSolRho[kk]
				p = (gamma-1.0)*(intSolEner[kk] - 0.5*intSolRho[kk]*(u*u+v*v))
				rH = intSolEner[kk]+p

				fluxF1[kk] = intSolRhou[kk]
				fluxF2[kk] = intSolRhou[kk]*u + p
				fluxF3[kk] = intSolRhou[kk]*v
				fluxF4[kk] = rH*u
				fluxG1[kk] = intSolRhov[kk]
				fluxG2[kk] = intSolRhov[kk]*u
				fluxG3[kk] = intSolRhov[kk]*v + p
				fluxG4[kk] = rH*v
			end

			-- Calculate parametric derivatives of fluxes
			-- in integration points
			for kk=0,nSpaceInt do
				gradFluxesIntFr1[kk] = 0.0
				gradFluxesIntFr2[kk] = 0.0
				gradFluxesIntFr3[kk] = 0.0
				gradFluxesIntFr4[kk] = 0.0
				gradFluxesIntFs1[kk] = 0.0
				gradFluxesIntFs2[kk] = 0.0
				gradFluxesIntFs3[kk] = 0.0
				gradFluxesIntFs4[kk] = 0.0
				gradFluxesIntGr1[kk] = 0.0
				gradFluxesIntGr2[kk] = 0.0
				gradFluxesIntGr3[kk] = 0.0
				gradFluxesIntGr4[kk] = 0.0
				gradFluxesIntGs1[kk] = 0.0
				gradFluxesIntGs2[kk] = 0.0
				gradFluxesIntGs3[kk] = 0.0
				gradFluxesIntGs4[kk] = 0.0

				for ll=0,Np do
					gradFluxesIntFr1[kk] += DrSpaceInt[kk*Np+ll].v*fluxF1[ll]
					gradFluxesIntFr2[kk] += DrSpaceInt[kk*Np+ll].v*fluxF2[ll]
					gradFluxesIntFr3[kk] += DrSpaceInt[kk*Np+ll].v*fluxF3[ll]
					gradFluxesIntFr4[kk] += DrSpaceInt[kk*Np+ll].v*fluxF4[ll]
					gradFluxesIntFs1[kk] += DsSpaceInt[kk*Np+ll].v*fluxF1[ll]
					gradFluxesIntFs2[kk] += DsSpaceInt[kk*Np+ll].v*fluxF2[ll]
					gradFluxesIntFs3[kk] += DsSpaceInt[kk*Np+ll].v*fluxF3[ll]
					gradFluxesIntFs4[kk] += DsSpaceInt[kk*Np+ll].v*fluxF4[ll]
					gradFluxesIntGr1[kk] += DrSpaceInt[kk*Np+ll].v*fluxG1[ll]
					gradFluxesIntGr2[kk] += DrSpaceInt[kk*Np+ll].v*fluxG2[ll]
					gradFluxesIntGr3[kk] += DrSpaceInt[kk*Np+ll].v*fluxG3[ll]
					gradFluxesIntGr4[kk] += DrSpaceInt[kk*Np+ll].v*fluxG4[ll]
					gradFluxesIntGs1[kk] += DsSpaceInt[kk*Np+ll].v*fluxG1[ll]
					gradFluxesIntGs2[kk] += DsSpaceInt[kk*Np+ll].v*fluxG2[ll]
					gradFluxesIntGs3[kk] += DsSpaceInt[kk*Np+ll].v*fluxG3[ll]
					gradFluxesIntGs4[kk] += DsSpaceInt[kk*Np+ll].v*fluxG4[ll]
				end
			end

			-- Calculate residuals at solDOFs
			for kk=0,nSpaceInt do
				resDumRho[kk]	= wSpaceInt[kk].v*( gradFluxesIntFr1[kk]*q[cellNum].rxInt[kk] + gradFluxesIntFs1[kk]*q[cellNum].sxInt[kk] + gradFluxesIntGr1[kk]*q[cellNum].ryInt[kk] + gradFluxesIntGs1[kk]*q[cellNum].syInt[kk] )
				resDumRhou[kk]	= wSpaceInt[kk].v*( gradFluxesIntFr2[kk]*q[cellNum].rxInt[kk] + gradFluxesIntFs2[kk]*q[cellNum].sxInt[kk] + gradFluxesIntGr2[kk]*q[cellNum].ryInt[kk] + gradFluxesIntGs2[kk]*q[cellNum].syInt[kk] )
				resDumRhov[kk]	= wSpaceInt[kk].v*( gradFluxesIntFr3[kk]*q[cellNum].rxInt[kk] + gradFluxesIntFs3[kk]*q[cellNum].sxInt[kk] + gradFluxesIntGr3[kk]*q[cellNum].ryInt[kk] + gradFluxesIntGs3[kk]*q[cellNum].syInt[kk] )
				resDumEner[kk]	= wSpaceInt[kk].v*( gradFluxesIntFr4[kk]*q[cellNum].rxInt[kk] + gradFluxesIntFs4[kk]*q[cellNum].sxInt[kk] + gradFluxesIntGr4[kk]*q[cellNum].ryInt[kk] + gradFluxesIntGs4[kk]*q[cellNum].syInt[kk] )
			end

			for kk=0,Np do
				resIntRho[kk]	= 0.0
				resIntRhou[kk]	= 0.0
				resIntRhov[kk]	= 0.0
				resIntEner[kk]	= 0.0
				for ll=0,nSpaceInt do
					resIntRho[kk]	+= DOFToIntSpaceTranspose[kk*nSpaceInt+ll].v*resDumRho[ll]
					resIntRhou[kk]	+= DOFToIntSpaceTranspose[kk*nSpaceInt+ll].v*resDumRhou[ll]
					resIntRhov[kk]	+= DOFToIntSpaceTranspose[kk*nSpaceInt+ll].v*resDumRhov[ll]
					resIntEner[kk]	+= DOFToIntSpaceTranspose[kk*nSpaceInt+ll].v*resDumEner[ll]
				end
			end

			-- Update the total residual by resInt
			-- calculated at current temporal integration point
			for kk=0,Nt do
				w = 0.5*wTimeInt[jj].v*dt*DOFToIntTime[jj*Nt+kk].v
				for ll=0,Np do
					resTotRho[kk*Np+ll]	-= w*resIntRho[ll]
					resTotRhou[kk*Np+ll]-= w*resIntRhou[ll]
					resTotRhov[kk*Np+ll]-= w*resIntRhov[ll]
					resTotEner[kk*Np+ll]-= w*resIntEner[ll]
				end
			end
		end

		-- Solve for the new values of predictor solution
		indVal = 0
		for jj=0,Np*Nt do
			 preSolRho[indVal]  = 0.0
			 preSolRhou[indVal]	= 0.0
			 preSolRhov[indVal]	= 0.0
			 preSolEner[indVal]	= 0.0

			for kk=0,Np*Nt do
				preSolRho[indVal]  += AderIterMat[jj*NtNp+kk].v*resTotRho[kk]
				preSolRhou[indVal] += AderIterMat[jj*NtNp+kk].v*resTotRhou[kk]
				preSolRhov[indVal] += AderIterMat[jj*NtNp+kk].v*resTotRhov[kk]
				preSolEner[indVal] += AderIterMat[jj*NtNp+kk].v*resTotEner[kk]
			end
			indVal = indVal + 1
		end
		cntTot += 1
		isConverged = true

		-- Check convergence criteria
		-- rho
		var dum : double = 0.0
		indVal = 0
		for jj=0,Np*Nt do
			dum += pow((preSolRho[indVal]-oldSolRho[jj]),2.0)
			indVal = indVal + 1
		end
		dum = sqrt(dum/(Np*Nt))
		if ( dum > tolSolAderRho ) then
			isConverged = false
		end

		-- rhou
		dum = 0.0
		indVal = 0
		for jj=0,Np*Nt do
			dum += pow((preSolRhou[indVal]-oldSolRhou[jj]),2.0)
			indVal = indVal + 1
		end
		dum = sqrt(dum/(Np*Nt))
		if ( dum > tolSolAderRhouRhov ) then
			isConverged = false
		end

		-- rhov
		dum = 0.0
		indVal = 0
		for jj=0,Np*Nt do
			dum += pow((preSolRhov[indVal]-oldSolRhov[jj]),2.0)
			indVal = indVal + 1
		end
		dum = sqrt(dum/(Np*Nt))
		if ( dum > tolSolAderRhouRhov ) then
			isConverged = false
		end

		-- ener
		dum = 0.0
		indVal = 0
		for jj=0,Np*Nt do
			dum += pow((preSolEner[indVal]-oldSolEner[jj]),2.0)
			indVal = indVal + 1
		end
		dum = sqrt(dum/(Np*Nt))
		if ( dum > tolSolAderEner ) then
			isConverged = false
		end
		if isConverged then
			break
		end
	end	
	if ( cnt > 90 ) then
		c.printf("Euler2DPredictor step is failed in cellNum = %10llu\n",cellNum)
		c.printf("TERMINATE THE PROGRAM..")
		c.abort()
	end
end

task Euler2DPredictorWrapper(p_space : int8, Np : uint64, Nt : uint64, nSpaceInt : uint64, nTimeInt : uint64, dt : double, tolSolAderRho : double, tolSolAderRhouRhov : double, tolSolAderEner : double, MSpace : region(ispace(int1d), doubleVal), DrSpaceInt : region(ispace(int1d), doubleVal), DsSpaceInt : region(ispace(int1d), doubleVal), wSpaceInt : region(ispace(int1d), doubleVal), DOFToIntSpaceTranspose : region(ispace(int1d), doubleVal), lFirst : region(ispace(int1d), doubleVal), wTimeInt : region(ispace(int1d), doubleVal), DOFToIntTime : region(ispace(int1d), doubleVal), AderIterMat : region(ispace(int1d),doubleVal), vmapM : region(ispace(int1d),uintVal), q : region(ispace(ptr), Elem), qHalo : region(ispace(ptr),Elem), QMFace : region(ispace(ptr),Surface), QPFace : region(ispace(ptr),Surface))
where
    reads(MSpace.v, DrSpaceInt.v, DsSpaceInt.v, wSpaceInt.v, DOFToIntSpaceTranspose.v, lFirst.v, wTimeInt.v, DOFToIntTime.v, AderIterMat.v, vmapM.v, q.sol, q.preSol, q.rxInt, q.sxInt, q.ryInt, q.syInt, q.QPInfo, q.adjQP, q.cellInd, qHalo.sol, qHalo.rxInt, qHalo.sxInt, qHalo.ryInt, qHalo.syInt, qHalo.QPInfo, QMFace.rho, QMFace.rhou, QMFace.rhov, QMFace.ener, QPFace.rho, QPFace.rhou, QPFace.rhov, QPFace.ener),
	writes(q.preSol, QMFace.rho, QMFace.rhou, QMFace.rhov, QMFace.ener, QPFace.rho, QPFace.rhou, QPFace.rhov, QPFace.ener)
do
	var cellNum			: uint64
	var cellNumTemp		: uint64
	var indVal			: uint64	
	var preSolRho		: double[220]		-- Np*Nt, Ps=9, Pt=3
	var preSolRhou		: double[220]		-- Np*Nt
	var preSolRhov		: double[220]		-- Np*Nt
	var preSolEner		: double[220]		-- Np*Nt
	var preSolRhoTemp	: double[220]		-- Np*Nt
	var preSolRhouTemp	: double[220]		-- Np*Nt
	var preSolRhovTemp	: double[220]		-- Np*Nt
	var preSolEnerTemp	: double[220]		-- Np*Nt
	var solIntRho		: double[55]		-- Np, Ps=9
	var solIntRhou		: double[55]		-- Np
	var solIntRhov		: double[55]		-- Np
	var solIntEner		: double[55]		-- Np
	var solIntRhoTemp	: double[55]		-- Np
	var solIntRhouTemp	: double[55]		-- Np
	var solIntRhovTemp	: double[55]		-- Np
	var solIntEnerTemp	: double[55]		-- Np
	var Nfp				: uint64 = p_space+1

	for e in q do
		cellNum = q[e].cellInd
		-- Solve predictor solution for this element 
		Euler2DPredictor(cellNum,Np,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpace,DrSpaceInt,DsSpaceInt,wSpaceInt,DOFToIntSpaceTranspose,lFirst,wTimeInt,DOFToIntTime,AderIterMat,q,preSolRho,preSolRhou,preSolRhov,preSolEner)

		-- Save predictor solutions
		for ii=0,Np*Nt do
			q[cellNum].preSol[ii].rho  = preSolRho[ii] 
			q[cellNum].preSol[ii].rhou = preSolRhou[ii]
			q[cellNum].preSol[ii].rhov = preSolRhov[ii]
			q[cellNum].preSol[ii].ener = preSolEner[ii]
		end

		---- Surface Traces
		-- Interpolate the predictor solution to the current temporal integration point
		for ii=0,nTimeInt do
			for kk=0,Np do
				solIntRho[kk]  = 0.0
				solIntRhou[kk] = 0.0
				solIntRhov[kk] = 0.0
				solIntEner[kk] = 0.0
			end
			indVal = 0
			for kk=0,Nt do
				for ll=0,Np do
					solIntRho[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRho[indVal] 
					solIntRhou[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhou[indVal]
					solIntRhov[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhov[indVal]
					solIntEner[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolEner[indVal]
					indVal = indVal + 1
				end
			end

			-- Store QM and QP Info for surface residuals
			-- Sweep 1
			for kk=0,Nfp do
				QMFace[cellNum].rho[ii][kk]	= solIntRho[vmapM[kk].v]
				QMFace[cellNum].rhou[ii][kk]= solIntRhou[vmapM[kk].v]
				QMFace[cellNum].rhov[ii][kk]= solIntRhov[vmapM[kk].v]
				QMFace[cellNum].ener[ii][kk]= solIntEner[vmapM[kk].v]
				if ( q[cellNum].adjQP[0] ) then  -- Target cell is in the same partition
					QPFace[q[cellNum].QPInfo[3][kk]].rho[ii][q[cellNum].QPInfo[2][kk]]	= solIntRho[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].rhou[ii][q[cellNum].QPInfo[2][kk]]	= solIntRhou[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].rhov[ii][q[cellNum].QPInfo[2][kk]]	= solIntRhov[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].ener[ii][q[cellNum].QPInfo[2][kk]]	= solIntEner[q[cellNum].QPInfo[0][kk]]
				end
			end

			-- Sweep 2
			for kk=Nfp,2*Nfp do
				QMFace[cellNum].rho[ii][kk]	= solIntRho[vmapM[kk].v]
				QMFace[cellNum].rhou[ii][kk]= solIntRhou[vmapM[kk].v]
				QMFace[cellNum].rhov[ii][kk]= solIntRhov[vmapM[kk].v]
				QMFace[cellNum].ener[ii][kk]= solIntEner[vmapM[kk].v]
				if ( q[cellNum].adjQP[1] ) then  -- Target cell is in the same partition 
					QPFace[q[cellNum].QPInfo[3][kk]].rho[ii][q[cellNum].QPInfo[2][kk]] = solIntRho[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].rhou[ii][q[cellNum].QPInfo[2][kk]] = solIntRhou[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].rhov[ii][q[cellNum].QPInfo[2][kk]] = solIntRhov[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].ener[ii][q[cellNum].QPInfo[2][kk]] = solIntEner[q[cellNum].QPInfo[0][kk]]
				end
			end

			-- Sweep 3
			for kk=2*Nfp,3*Nfp do
				QMFace[cellNum].rho[ii][kk]	= solIntRho[vmapM[kk].v]
				QMFace[cellNum].rhou[ii][kk]= solIntRhou[vmapM[kk].v]
				QMFace[cellNum].rhov[ii][kk]= solIntRhov[vmapM[kk].v]
				QMFace[cellNum].ener[ii][kk]= solIntEner[vmapM[kk].v]
				if ( q[cellNum].adjQP[2] ) then  -- Target cell is in the same partition
					QPFace[q[cellNum].QPInfo[3][kk]].rho[ii][q[cellNum].QPInfo[2][kk]]	= solIntRho[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].rhou[ii][q[cellNum].QPInfo[2][kk]]	= solIntRhou[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].rhov[ii][q[cellNum].QPInfo[2][kk]]	= solIntRhov[q[cellNum].QPInfo[0][kk]]
					QPFace[q[cellNum].QPInfo[3][kk]].ener[ii][q[cellNum].QPInfo[2][kk]]	= solIntEner[q[cellNum].QPInfo[0][kk]]
				end
			end
		end

		if not ( q[cellNum].adjQP[0] ) then	-- Target cell is not in the same partition. Need to do other work
			cellNumTemp = q[cellNum].QPInfo[3][0*Nfp]
			Euler2DPredictor(cellNumTemp,Np,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpace,DrSpaceInt,DsSpaceInt,wSpaceInt,DOFToIntSpaceTranspose,lFirst,wTimeInt,DOFToIntTime,AderIterMat,qHalo,preSolRhoTemp,preSolRhouTemp,preSolRhovTemp,preSolEnerTemp)

			for ii=0,nTimeInt do			
				for kk=0,Np do
					solIntRhoTemp[kk]  = 0.0
					solIntRhouTemp[kk] = 0.0
					solIntRhovTemp[kk] = 0.0
					solIntEnerTemp[kk] = 0.0
				end
	
				indVal = 0
				for kk=0,Nt do
					for ll=0,Np do
						solIntRhoTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhoTemp[indVal]
						solIntRhouTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhouTemp[indVal]
						solIntRhovTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhovTemp[indVal]
						solIntEnerTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolEnerTemp[indVal]
						indVal = indVal + 1
					end
				end

				for kk=0,3*Nfp do
					if ( qHalo[cellNumTemp].QPInfo[3][kk] == cellNum ) then
						QPFace[cellNum].rho[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhoTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].rhou[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhouTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].rhov[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhovTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].ener[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntEnerTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
					end
				end
			end
		end

		if not ( q[cellNum].adjQP[1] ) then	-- Target cell is not in the same partition. Need to do other work
			cellNumTemp = q[cellNum].QPInfo[3][1*Nfp]
			Euler2DPredictor(cellNumTemp,Np,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpace,DrSpaceInt,DsSpaceInt,wSpaceInt,DOFToIntSpaceTranspose,lFirst,wTimeInt,DOFToIntTime,AderIterMat,qHalo,preSolRhoTemp,preSolRhouTemp,preSolRhovTemp,preSolEnerTemp)

			for ii=0,nTimeInt do
				for kk=0,Np do
					solIntRhoTemp[kk]  = 0.0
					solIntRhouTemp[kk] = 0.0
					solIntRhovTemp[kk] = 0.0
					solIntEnerTemp[kk] = 0.0
				end
	
				indVal = 0
				for kk=0,Nt do
					for ll=0,Np do
						solIntRhoTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhoTemp[indVal]
						solIntRhouTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhouTemp[indVal]
						solIntRhovTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhovTemp[indVal]
						solIntEnerTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolEnerTemp[indVal]
						indVal = indVal + 1
					end
				end

				for kk=0,3*Nfp do
					if ( qHalo[cellNumTemp].QPInfo[3][kk] == cellNum ) then
						QPFace[cellNum].rho[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhoTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].rhou[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhouTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].rhov[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhovTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].ener[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntEnerTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
					end
				end
			end
		end

		if not ( q[cellNum].adjQP[2] ) then	-- Target cell is not in the same partition. Need to do other work
			cellNumTemp = q[cellNum].QPInfo[3][2*Nfp]
			Euler2DPredictor(cellNumTemp,Np,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpace,DrSpaceInt,DsSpaceInt,wSpaceInt,DOFToIntSpaceTranspose,lFirst,wTimeInt,DOFToIntTime,AderIterMat,qHalo,preSolRhoTemp,preSolRhouTemp,preSolRhovTemp,preSolEnerTemp)

			for ii=0,nTimeInt do	
				for kk=0,Np do
					solIntRhoTemp[kk]  = 0.0
					solIntRhouTemp[kk] = 0.0
					solIntRhovTemp[kk] = 0.0
					solIntEnerTemp[kk] = 0.0
				end
	
				indVal = 0
				for kk=0,Nt do
					for ll=0,Np do
						solIntRhoTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhoTemp[indVal]
						solIntRhouTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhouTemp[indVal]
						solIntRhovTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolRhovTemp[indVal]
						solIntEnerTemp[ll]	+= DOFToIntTime[ii*Nt+kk].v*preSolEnerTemp[indVal]
						indVal = indVal + 1
					end
				end

				for kk=0,3*Nfp do
					if ( qHalo[cellNumTemp].QPInfo[3][kk] == cellNum ) then
						QPFace[cellNum].rho[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhoTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].rhou[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhouTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].rhov[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntRhovTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
						QPFace[cellNum].ener[ii][qHalo[cellNumTemp].QPInfo[2][kk]]	= solIntEnerTemp[qHalo[cellNumTemp].QPInfo[0][kk]]
					end
				end
			end
		end
	end
end

terra Euler2DFluxes(Np : uint64, F1 : &double, F2 : &double, F3 : &double, F4 : &double, G1 : &double, G2 : &double, G3 : &double, G4 : &double, intSolRho : &double, intSolRhou : &double, intSolRhov : &double, intSolEner : &double)
	var u : double
	var v : double
	var p : double
	var gamma : double = 1.4

	for ii=0,Np do
		u = intSolRhou[ii]/intSolRho[ii]
		v = intSolRhov[ii]/intSolRho[ii]
		p = (gamma-1.0)*(intSolEner[ii] - 0.5*intSolRho[ii]*( u*u + v*v ))
		F1[ii] = intSolRhou[ii]
		F2[ii] = intSolRhou[ii]*u + p
		F3[ii] = intSolRhov[ii]*u
		F4[ii] = u*(intSolEner[ii]+p)

		G1[ii] = intSolRhov[ii]
		G2[ii] = intSolRhou[ii]*v
		G3[ii] = intSolRhov[ii]*v + p
		G4[ii] = v*(intSolEner[ii]+p)
	end
	return
end

__demand(__inline)
task Euler2DLF(cellInd : uint64, p_space : int8, F1 : &double, F2 : &double, F3 : &double, F4 : &double, nx : &double, ny : &double, QMRho : &double, QMRhou : &double, QMRhov : &double, QMEner : &double, QPRho : &double, QPRhou : &double, QPRhov : &double, QPEner : &double)
	var maxVelCand : double
	var maxVelTemp : double
	var gamma	: double = 1.4
	var Nfp		: uint64 = p_space+1 
	var maxVel	: double[30]	-- Nfaces*Nfp, Ps=9
	var rhoM	: double[30]	-- Nfaces*Nfp
	var rhouM	: double[30]	-- Nfaces*Nfp
	var rhovM	: double[30]	-- Nfaces*Nfp
	var enerM	: double[30]	-- Nfaces*Nfp
	var uM		: double[30]	-- Nfaces*Nfp
	var vM		: double[30]	-- Nfaces*Nfp
	var pM		: double[30]	-- Nfaces*Nfp
    var rhoP    : double[30]	-- Nfaces*Nfp
    var rhouP   : double[30]	-- Nfaces*Nfp
    var rhovP   : double[30]	-- Nfaces*Nfp
    var enerP   : double[30]	-- Nfaces*Nfp
	var uP		: double[30]	-- Nfaces*Nfp
	var vP		: double[30]	-- Nfaces*Nfp
	var pP		: double[30]	-- Nfaces*Nfp

	var F1M		: double[30]	-- Nfaces*Nfp
    var F2M		: double[30]	-- Nfaces*Nfp
    var F3M		: double[30]	-- Nfaces*Nfp
    var F4M		: double[30]	-- Nfaces*Nfp
    var G1M		: double[30]	-- Nfaces*Nfp
    var G2M		: double[30]	-- Nfaces*Nfp
    var G3M		: double[30]	-- Nfaces*Nfp
    var G4M		: double[30]	-- Nfaces*Nfp
    var F1P     : double[30]	-- Nfaces*Nfp
    var F2P     : double[30]	-- Nfaces*Nfp
    var F3P     : double[30]	-- Nfaces*Nfp
    var F4P     : double[30]	-- Nfaces*Nfp
    var G1P     : double[30]	-- Nfaces*Nfp
    var G2P     : double[30]	-- Nfaces*Nfp
    var G3P     : double[30]	-- Nfaces*Nfp
    var G4P     : double[30]	-- Nfaces*Nfp

	for ii=0,3*Nfp do
		rhoM[ii]  = QMRho[ii]
		rhouM[ii] = QMRhou[ii]
        rhovM[ii] = QMRhov[ii]
		enerM[ii] = QMEner[ii]
		uM[ii] = rhouM[ii]/rhoM[ii]
		vM[ii] = rhovM[ii]/rhoM[ii]
		pM[ii] = (gamma-1.0)*(enerM[ii] - 0.5*rhoM[ii]*(uM[ii]*uM[ii]+vM[ii]*vM[ii]))

        rhoP[ii]  = QPRho[ii]
        rhouP[ii] = QPRhou[ii]
        rhovP[ii] = QPRhov[ii]
        enerP[ii] = QPEner[ii]
        uP[ii] = rhouP[ii]/rhoP[ii]
        vP[ii] = rhovP[ii]/rhoP[ii]
        pP[ii] = (gamma-1.0)*(enerP[ii] - 0.5*rhoP[ii]*(uP[ii]*uP[ii]+vP[ii]*vP[ii]))
	end

	-- Compute fluxes
	Euler2DFluxes(3*Nfp,F1M,F2M,F3M,F4M,G1M,G2M,G3M,G4M,rhoM,rhouM,rhovM,enerM)
	Euler2DFluxes(3*Nfp,F1P,F2P,F3P,F4P,G1P,G2P,G3P,G4P,rhoP,rhouP,rhovP,enerP)

	-- Compute wave speed for Lax-Friedrichs(Rusanov) numerical fluxes
	for ii=0,3 do
		maxVelCand = 0.0
		for jj=0,Nfp do
			maxVelTemp = max( (sqrt(uM[ii*Nfp+jj]*uM[ii*Nfp+jj]+vM[ii*Nfp+jj]*vM[ii*Nfp+jj])+sqrt(abs(gamma*pM[ii*Nfp+jj]/rhoM[ii*Nfp+jj]))) , (sqrt(uP[ii*Nfp+jj]*uP[ii*Nfp+jj]+vP[ii*Nfp+jj]*vP[ii*Nfp+jj])+sqrt(abs(gamma*pP[ii*Nfp+jj]/rhoP[ii*Nfp+jj]))) )
			maxVelCand = max(maxVelCand,maxVelTemp)
		end
		for jj=0,Nfp do
			maxVel[ii*Nfp+jj] = maxVelCand
		end
	end

	-- Form LF fluxes
	for ii=0,3*Nfp do
		F1[ii] = 0.5*( nx[ii]*(F1P[ii]+F1M[ii]) + ny[ii]*(G1P[ii]+G1M[ii]) + maxVel[ii]*(rhoM[ii]-rhoP[ii]))
		F2[ii] = 0.5*( nx[ii]*(F2P[ii]+F2M[ii]) + ny[ii]*(G2P[ii]+G2M[ii]) + maxVel[ii]*(rhouM[ii]-rhouP[ii]))
		F3[ii] = 0.5*( nx[ii]*(F3P[ii]+F3M[ii]) + ny[ii]*(G3P[ii]+G3M[ii]) + maxVel[ii]*(rhovM[ii]-rhovP[ii]))
		F4[ii] = 0.5*( nx[ii]*(F4P[ii]+F4M[ii]) + ny[ii]*(G4P[ii]+G4M[ii]) + maxVel[ii]*(enerM[ii]-enerP[ii]))
	end
end

task Euler2DCorrector(p_space : int8, Np : uint64, Nt : uint64, nTimeInt : uint64, Drw : region(ispace(int1d),doubleVal), Dsw : region(ispace(int1d),doubleVal), LIFT : region(ispace(int1d),doubleVal), wTimeInt : region(ispace(int1d),doubleVal), DOFToIntTime : region(ispace(int1d),doubleVal), q : region(ispace(ptr),Elem), QMFace : region(ispace(ptr),Surface), QPFace : region(ispace(ptr),Surface))
where
	reads(Drw.v, Dsw.v, LIFT.v, wTimeInt.v, DOFToIntTime.v, q.cellInd, q.volRes, q.surfRes, q.preSol, q.rx, q.sx, q.ry, q.sy, q.nx, q.ny, q.Fscale, q.QPInfo, QMFace.rho, QMFace.rhou, QMFace.rhov, QMFace.ener, QPFace.rho, QPFace.rhou, QPFace.rhov, QPFace.ener),
	writes(q.volRes, q.surfRes)
do
	var solIntRho		: double[55]		-- Np, Ps=9
	var solIntRhou		: double[55]		-- Np
	var solIntRhov		: double[55]		-- Np
	var solIntEner		: double[55]		-- Np
	var F1				: double[55]		-- Np
	var F2				: double[55]		-- Np
	var F3				: double[55]		-- Np
	var F4				: double[55]		-- Np
	var G1				: double[55]		-- Np
	var G2				: double[55]		-- Np
	var G3				: double[55]		-- Np
	var G4				: double[55]		-- Np
	var F1s				: double[30]		-- Nfaces*Nfp, Ps=9 
	var F2s				: double[30]		-- Nfaces*Nfp
	var F3s				: double[30]		-- Nfaces*Nfp
	var F4s				: double[30]		-- Nfaces*Nfp
	var QMRho			: double[30]		-- Nfaces*Nfp
	var QMRhou			: double[30]		-- Nfaces*Nfp
	var QMRhov			: double[30]		-- Nfaces*Nfp
	var QMEner			: double[30]		-- Nfaces*Nfp
	var QPRho			: double[30]		-- Nfaces*Nfp
	var QPRhou			: double[30]		-- Nfaces*Nfp
	var QPRhov			: double[30]		-- Nfaces*Nfp
	var QPEner			: double[30]		-- Nfaces*Nfp
	var dFdr			: double[55]		-- Np, Ps=9 
	var dFds			: double[55]		-- Np
	var dGdr			: double[55]		-- Np
	var dGds			: double[55]		-- Np
	var Nfp				: uint64 = p_space+1
	var NfpSum			: uint64 = 3*Nfp 
	var gamma			: double = 1.4
	var w				: double
	var tempVal			: double
	var cellNum			: uint64

	for e in q do
		cellNum = q[e].cellInd
		-- Initialize volume and surface residuals to zero for corrector step
        for ii=0,Np do
            q[cellNum].volRes[ii].rho = 0.0
            q[cellNum].volRes[ii].rhou = 0.0
            q[cellNum].volRes[ii].rhov = 0.0
            q[cellNum].volRes[ii].ener = 0.0
            q[cellNum].surfRes[ii].rho = 0.0
            q[cellNum].surfRes[ii].rhou = 0.0
            q[cellNum].surfRes[ii].rhov = 0.0
            q[cellNum].surfRes[ii].ener = 0.0    
        end
		-- Looping over temporal integration points
		for ii=0,nTimeInt do
			-- Weight for residual calculation
			w = 0.5*wTimeInt[ii].v

			-- Interpolate the predictor solution to the current temporal integration point
			for kk=0,Np do
				solIntRho[kk]  = 0.0
				solIntRhou[kk] = 0.0
				solIntRhov[kk] = 0.0
				solIntEner[kk] = 0.0
			end
			for kk=0,Nt do
				for ll=0,Np do
					solIntRho[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[cellNum].preSol[kk*Np+ll].rho
					solIntRhou[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[cellNum].preSol[kk*Np+ll].rhou
					solIntRhov[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[cellNum].preSol[kk*Np+ll].rhov
					solIntEner[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[cellNum].preSol[kk*Np+ll].ener
				end
			end

			---- Volume residual
			-- Calculate flux
			Euler2DFluxes(Np,F1,F2,F3,F4,G1,G2,G3,G4,solIntRho,solIntRhou,solIntRhov,solIntEner)

			-- Rho
			for kk=0, Np do
				dFdr[kk] = 0.0
				dFds[kk] = 0.0
				dGdr[kk] = 0.0
				dGds[kk] = 0.0
				for ll=0, Np do
					dFdr[kk] += Drw[kk*Np+ll].v*F1[ll]
					dFds[kk] += Dsw[kk*Np+ll].v*F1[ll]
					dGdr[kk] += Drw[kk*Np+ll].v*G1[ll]
					dGds[kk] += Dsw[kk*Np+ll].v*G1[ll]
				end
				q[cellNum].volRes[kk].rho += w*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
			end

			-- Rhou
			for kk=0, Np do
				dFdr[kk] = 0.0
				dFds[kk] = 0.0
				dGdr[kk] = 0.0
				dGds[kk] = 0.0
				for ll=0, Np do
					dFdr[kk] += Drw[kk*Np+ll].v*F2[ll]
					dFds[kk] += Dsw[kk*Np+ll].v*F2[ll]
					dGdr[kk] += Drw[kk*Np+ll].v*G2[ll]
					dGds[kk] += Dsw[kk*Np+ll].v*G2[ll]
				end
				q[cellNum].volRes[kk].rhou += w*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
			end

			-- Rhov
			for kk=0, Np do
				dFdr[kk] = 0.0
				dFds[kk] = 0.0
				dGdr[kk] = 0.0
				dGds[kk] = 0.0
				for ll=0, Np do
					dFdr[kk] += Drw[kk*Np+ll].v*F3[ll]
					dFds[kk] += Dsw[kk*Np+ll].v*F3[ll]
					dGdr[kk] += Drw[kk*Np+ll].v*G3[ll]
					dGds[kk] += Dsw[kk*Np+ll].v*G3[ll]
				end
				q[cellNum].volRes[kk].rhov += w*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
			end

			-- Ener
			for kk=0, Np do
				dFdr[kk] = 0.0
				dFds[kk] = 0.0
				dGdr[kk] = 0.0
				dGds[kk] = 0.0
				for ll=0, Np do
					dFdr[kk] += Drw[kk*Np+ll].v*F4[ll]
					dFds[kk] += Dsw[kk*Np+ll].v*F4[ll]
					dGdr[kk] += Drw[kk*Np+ll].v*G4[ll]
					dGds[kk] += Dsw[kk*Np+ll].v*G4[ll]
				end
				q[cellNum].volRes[kk].ener += w*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
			end


			---- Surface residual
			-- Calculate surface fluxes
			Euler2DLF(cellNum, p_space, F1s, F2s, F3s, F4s, q[cellNum].nx, q[cellNum].ny, QMFace[cellNum].rho[ii], QMFace[cellNum].rhou[ii], QMFace[cellNum].rhov[ii], QMFace[cellNum].ener[ii], QPFace[cellNum].rho[ii], QPFace[cellNum].rhou[ii], QPFace[cellNum].rhov[ii], QPFace[cellNum].ener[ii])

			for kk=0,Np do
				-- Rho
				tempVal = 0.0
				for ll=0,3*Nfp do
					tempVal -= LIFT[kk*NfpSum+ll].v*q[cellNum].Fscale[ll]*F1s[ll]
				end
				q[cellNum].surfRes[kk].rho += (w*tempVal)

				-- Rhou
				tempVal = 0.0
				for ll=0,3*Nfp do
					tempVal -= LIFT[kk*NfpSum+ll].v*q[cellNum].Fscale[ll]*F2s[ll]
				end
				q[cellNum].surfRes[kk].rhou += (w*tempVal)

				-- Rhov
				tempVal = 0.0
				for ll=0,3*Nfp do
					tempVal -= LIFT[kk*NfpSum+ll].v*q[cellNum].Fscale[ll]*F3s[ll]
				end
				q[cellNum].surfRes[kk].rhov += (w*tempVal)

				-- Ener
				tempVal = 0.0
				for ll=0,3*Nfp do
					tempVal -= LIFT[kk*NfpSum+ll].v*q[cellNum].Fscale[ll]*F4s[ll]
				end
				q[cellNum].surfRes[kk].ener += (w*tempVal)
			end
		end
	end
end

task Euler2DUpdateSolution(dt : double, Np : uint64, q : region(ispace(ptr),Elem))
where
	reads(q.sol,q.volRes,q.surfRes,q.cellInd),
	writes(q.sol)
do
	var cellNum : uint64
	for e in q do
		cellNum = q[e].cellInd
		for kk=0,Np do
			q[cellNum].sol[kk].rho  += dt*(q[cellNum].volRes[kk].rho + q[cellNum].surfRes[kk].rho)
			q[cellNum].sol[kk].rhou += dt*(q[cellNum].volRes[kk].rhou + q[cellNum].surfRes[kk].rhou)
			q[cellNum].sol[kk].rhov += dt*(q[cellNum].volRes[kk].rhov + q[cellNum].surfRes[kk].rhov)
			q[cellNum].sol[kk].ener += dt*(q[cellNum].volRes[kk].ener + q[cellNum].surfRes[kk].ener)
		end
	end
end

task calcL1Error(time : double, Np : uint64, epsVal : double, rho0Val : double, u0Val : double, v0Val : double, q : region(ispace(ptr), Elem)) 
where
    reads(q.cellInd, q.sol, q._x, q._y)
do
	var L1		: double = 0.0
	var temp	: double = 0.0
	var cellNum : uint64
	var gamma	: double = 1.4
	var epsilon	: double = epsVal
	var rho0	: double = rho0Val
	var u0		: double = u0Val
	var v0		: double = v0Val
	var xDis	: double
	var yDis	: double
	var r		: double
	var rho1	: double
	var u1		: double
	var v1		: double

	-- Calculate vortex center at current time
	var xCenter : double = 5.0 + u0*time
	var yCenter : double = 0.0 + v0*time
	var cnt		: uint64

	if ( xCenter > 10.0 ) then
		cnt = xCenter/10
		for ii=0,cnt do
			xCenter -= 10.0
			yCenter -= 10.0
		end
	end

	for e in q do
		cellNum = e.cellInd
		for jj=0,Np do
			xDis = q[cellNum]._x[jj] - xCenter
			yDis = q[cellNum]._y[jj] - yCenter
			r = sqrt( xDis*xDis + yDis*yDis)

			u1 = u0 - epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*yDis
			v1 = v0 + epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*xDis
			rho1 = pow(( 1.0 - (gamma-1.0)*epsilon*epsilon/(8.0*gamma*PI*PI)*cmath.exp(1.0-r*r)),(1.0/(gamma-1.0)))

			temp = q[cellNum].sol[jj].rho - rho1 
			L1 += abs(temp)
		end
	end
	return L1
end

task calcL2Error(time : double, Np : uint64, epsVal : double, rho0Val : double, u0Val : double, v0Val : double, q : region(ispace(ptr), Elem)) 
where
    reads(q.cellInd, q.sol, q._x, q._y)
do
	var L2		: double = 0.0
	var temp	: double = 0.0
	var cellNum : uint64
	var gamma	: double = 1.4
	var epsilon	: double = epsVal
	var rho0	: double = rho0Val
	var u0		: double = u0Val
	var v0		: double = v0Val
	var xDis	: double
	var yDis	: double
	var r		: double
	var rho1	: double
	var u1		: double
	var v1		: double

	-- Calculate vortex center at current time
	var xCenter : double = 5.0 + u0*time
	var yCenter : double = 0.0 + v0*time
	var cnt		: uint64
	if ( xCenter > 10.0 ) then
		cnt = xCenter/10
		for ii=0,cnt do
			xCenter -= 10.0
			yCenter -= 10.0
		end
	end

	for e in q do
		cellNum = e.cellInd
		for jj=0,Np do
			xDis = q[cellNum]._x[jj] - xCenter
			yDis = q[cellNum]._y[jj] - yCenter
			r = sqrt( xDis*xDis + yDis*yDis)

			u1 = u0 - epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*yDis
			v1 = v0 + epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*xDis
			rho1 = pow(( 1.0 - (gamma-1.0)*epsilon*epsilon/(8.0*gamma*PI*PI)*cmath.exp(1.0-r*r)),(1.0/(gamma-1.0)))

			temp = q[cellNum].sol[jj].rho - rho1
			L2 += (temp*temp)
		end
	end
	return L2
end

task calcLInfError(time : double, Np : uint64, epsVal : double, rho0Val : double, u0Val : double, v0Val : double, q : region(ispace(ptr), Elem)) 
where
    reads(q.cellInd, q.sol, q._x, q._y)
do
	var LInf	: double = 0.0
	var temp	: double = 0.0
	var cellNum : uint64
	var gamma	: double = 1.4
	var epsilon	: double = epsVal
	var rho0	: double = rho0Val
	var u0		: double = u0Val
	var v0		: double = v0Val
	var xDis	: double
	var yDis	: double
	var r		: double
	var rho1	: double
	var u1		: double
	var v1		: double

	-- Calculate vortex center at current time
	var xCenter : double = 5.0 + u0*time
	var yCenter : double = 0.0 + v0*time
	var cnt		: uint64

	if ( xCenter > 10.0 ) then
		cnt = xCenter/10
		for ii=0,cnt do
			xCenter -= 10.0
			yCenter -= 10.0
		end
	end

	for e in q do
		cellNum = e.cellInd
		for jj=0,Np do
			xDis = q[cellNum]._x[jj] - xCenter
			yDis = q[cellNum]._y[jj] - yCenter
			r = sqrt( xDis*xDis + yDis*yDis)

			u1 = u0 - epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*yDis
			v1 = v0 + epsilon/(2.0*PI)*cmath.exp(0.5*(1.0-r*r))*xDis
			rho1 = pow(( 1.0 - (gamma-1.0)*epsilon*epsilon/(8.0*gamma*PI*PI)*cmath.exp(1.0-r*r)),(1.0/(gamma-1.0)))

			temp = q[cellNum].sol[jj].rho - rho1
			LInf = max(LInf, abs(temp))
		end
	end
	return LInf
end

task toplevel()
	-- 1) Declare several variables
    var nDOFs				: uint64
	var Nfp					: uint64
    var Nt					: uint64
	var nSpaceInt			: uint64
	var nTimeInt			: uint64
	var gridK				: uint64
	var gridNv				: uint64
	var dt					: double
	var tolSolAderRho		: double
	var tolSolAderRhouRhov	: double
	var tolSolAderEner		: double
	var simTime				: double = 0.0
	var finalTime			: double = 10.0		-- Default value
	var CFL					: double = 0.5		-- Default value
	var L1Error				: double = 0.0
	var L2Error				: double = 0.0
	var LInfError			: double = 0.0
	var token				: uint64 = 0
	var partFileNameLocal	: int8[64]


	-- 2) Get *.config input and assign values
	var config : su2Config
	config:initializeFromCommand()
	finalTime	= config.finalTime
	CFL			= config.CFL
	nDOFs		= NpG
	Nfp			= NfpG
	Nt			= NtG
	nSpaceInt	= nSpaceIntG
	nTimeInt	= nTimeIntG
	gridK		= gridKG
	gridNv		= gridNvG
	cstring.strcpy(partFileNameLocal,partFileName)
	cstring.strcat(partFileNameLocal,config.partFileTail)
	var colors		= ispace(int1d, config.parallelism)


	-- 3) Read standard element info based on p_space
	var MSpace		= region(ispace(int1d,nDOFs*nDOFs), doubleVal)
	var Dr			= region(ispace(int1d,nDOFs*nDOFs), doubleVal)
	var Ds			= region(ispace(int1d,nDOFs*nDOFs), doubleVal)
	var Drw			= region(ispace(int1d,nDOFs*nDOFs), doubleVal)
	var Dsw			= region(ispace(int1d,nDOFs*nDOFs), doubleVal)
	var LIFT		= region(ispace(int1d,(3*Nfp)*nDOFs), doubleVal)
	var DrSpaceInt	= region(ispace(int1d,nSpaceInt*nDOFs), doubleVal)
	var DsSpaceInt	= region(ispace(int1d,nSpaceInt*nDOFs), doubleVal)
	var wSpaceInt	= region(ispace(int1d,nSpaceInt), doubleVal)
	var DOFToIntSpaceTranspose= region(ispace(int1d,nSpaceInt*nDOFs), doubleVal)
	var lFirst		= region(ispace(int1d,Nt), doubleVal)
	var wTimeInt	= region(ispace(int1d,nTimeInt), doubleVal)
	var DOFToIntTime= region(ispace(int1d,nTimeInt*Nt), doubleVal)
	var AderIterMat	= region(ispace(int1d,(nDOFs*Nt)*(nDOFs*Nt)), doubleVal)
	var vmapM		= region(ispace(int1d,3*Nfp), uintVal)
	readStdElemSpaceInfo(stdElemSpaceFileName, MSpace, Dr, Ds, Drw, Dsw, LIFT, DrSpaceInt, DsSpaceInt, wSpaceInt, DOFToIntSpaceTranspose, vmapM)
	readStdElemTimeInfo(stdElemTimeFileName, lFirst, wTimeInt, DOFToIntTime)
	readAderIterMatInfo(aderIterMatFileName, AderIterMat)

	-- Create aliased partitions of regions for standard element
	-- info regions to SPMDize the code
	var coloring1 = c.legion_domain_point_coloring_create()
	var coloring2 = c.legion_domain_point_coloring_create()
	var coloring3 = c.legion_domain_point_coloring_create()
	var coloring4 = c.legion_domain_point_coloring_create()
	var coloring5 = c.legion_domain_point_coloring_create()
	var coloring6 = c.legion_domain_point_coloring_create()
	var coloring7 = c.legion_domain_point_coloring_create()
	var coloring8 = c.legion_domain_point_coloring_create()
	var coloring9 = c.legion_domain_point_coloring_create()

	for ii=0,config.parallelism do
		c.legion_domain_point_coloring_color_domain(coloring1, [int1d](ii), rect1d {0,nDOFs*nDOFs-1})		-- MSpace, Dr, Ds, Drw, Dsw
		c.legion_domain_point_coloring_color_domain(coloring2, [int1d](ii), rect1d {0,3*Nfp*nDOFs-1})		-- LIFT
		c.legion_domain_point_coloring_color_domain(coloring3, [int1d](ii), rect1d {0,nSpaceInt*nDOFs-1})	-- DrSpaceInt, DsSpaceInt, DOFToIntSpaceTranspose
		c.legion_domain_point_coloring_color_domain(coloring4, [int1d](ii), rect1d {0,nSpaceInt-1})			-- wSpaceInt
		c.legion_domain_point_coloring_color_domain(coloring5, [int1d](ii), rect1d {0,Nt-1})					-- lFirst
		c.legion_domain_point_coloring_color_domain(coloring6, [int1d](ii), rect1d {0,nTimeInt-1})				-- wTimeInt
		c.legion_domain_point_coloring_color_domain(coloring7, [int1d](ii), rect1d {0,nTimeInt*Nt-1})					-- DOFTonIntTime
		c.legion_domain_point_coloring_color_domain(coloring8, [int1d](ii), rect1d {0,nDOFs*Nt*nDOFs*Nt-1})	-- AderIterMat
		c.legion_domain_point_coloring_color_domain(coloring9, [int1d](ii), rect1d {0,3*Nfp-1})				-- vmapM
	end
	var MSpacePart					= partition(aliased,MSpace,coloring1,ispace(int1d,config.parallelism))
	var DrPart						= partition(aliased,Dr,coloring1,ispace(int1d,config.parallelism))
	var DsPart						= partition(aliased,Ds,coloring1,ispace(int1d,config.parallelism))
	var DrwPart						= partition(aliased,Drw,coloring1,ispace(int1d,config.parallelism))
	var DswPart						= partition(aliased,Dsw,coloring1,ispace(int1d,config.parallelism))
	var LIFTPart					= partition(aliased,LIFT,coloring2,ispace(int1d,config.parallelism))
	var DrSpaceIntPart				= partition(aliased,DrSpaceInt,coloring3,ispace(int1d,config.parallelism))
	var DsSpaceIntPart				= partition(aliased,DsSpaceInt,coloring3,ispace(int1d,config.parallelism))
	var DOFToIntSpaceTransposePart	= partition(aliased,DOFToIntSpaceTranspose,coloring3,ispace(int1d,config.parallelism))
	var wSpaceIntPart				= partition(aliased,wSpaceInt,coloring4,ispace(int1d,config.parallelism))
	var lFirstPart					= partition(aliased,lFirst,coloring5,ispace(int1d,config.parallelism))
	var wTimeIntPart				= partition(aliased,wTimeInt,coloring6,ispace(int1d,config.parallelism))
	var DOFToIntTimePart			= partition(aliased,DOFToIntTime,coloring7,ispace(int1d,config.parallelism))
	var AderIterMatPart				= partition(aliased,AderIterMat,coloring8,ispace(int1d,config.parallelism))
	var vmapMPart					= partition(aliased,vmapM,coloring9,ispace(int1d,config.parallelism))
	c.legion_domain_point_coloring_destroy(coloring1)
	c.legion_domain_point_coloring_destroy(coloring2)
	c.legion_domain_point_coloring_destroy(coloring3)
	c.legion_domain_point_coloring_destroy(coloring4)
	c.legion_domain_point_coloring_destroy(coloring5)
	c.legion_domain_point_coloring_destroy(coloring6)
	c.legion_domain_point_coloring_destroy(coloring7)
	c.legion_domain_point_coloring_destroy(coloring8)
	c.legion_domain_point_coloring_destroy(coloring9)


	-- 4) Read mesh
	var	gridVertex		= region(ispace(ptr,gridNv), GridVertex)
	var gridEToV		= region(ispace(ptr,gridK), GridEToV(wild))
	var gridVertexEqual	= partition(equal,gridVertex,colors)
	var gridEToVEqual	= partition(equal,gridEToV,colors)

    -- Read vertices info
	for color in colors do
		if ( [uint32](color) == 0 ) then
			c.printf("--------------Read Vertices Info------------\n\n")
		end
		readVertex(meshFileName,gridVertexEqual[color])
	end

	-- Read EToV info
	for color in colors do
		if ( [uint32](color) == 0 ) then
			c.printf("----------Read Elem To Vertex Info----------\n\n")
		end
		readElemToVertex(meshFileName,gridNv,gridVertex,gridEToVEqual[color])
	end


	-- 5) Declare regions
	var q			= region(ispace(ptr,gridK), Elem)
	var QMFace		= region(ispace(ptr,gridK), Surface)   
	var QPFace		= region(ispace(ptr,gridK), Surface)

	var qEqual		= partition(equal,q,colors)
	var QMFaceEqual	= partition(equal,QMFace,colors)
	var QPFaceEqual	= partition(equal,QPFace,colors)


	-- 6) Read connectivity info and graph partition info
	for color in colors do
		if ( [uint32](color) == 0 ) then
			c.printf("---------Generate Connectiviy Info----------\n\n")
		end
		generateQPConnectivity(EToEFileName, EToFFileName, Nfp, qEqual[color])
	end
	for color in colors do
		if ( [uint32](color) == 0 ) then
			c.printf("---------------Color Elements---------------\n\n")
		end
		colorElem(partFileNameLocal, config.parallelism, gridVertex, gridEToVEqual[color],qEqual[color])
	end
	for color in colors do
		if ( [uint32](color) == 0 ) then
			c.printf("-----------------Color Faces----------------\n\n")
		end
		colorFaces(Nfp, q, qEqual[color], QMFaceEqual[color], QPFaceEqual[color])
	end
	__fence(__execution, __block)


	-- 7) Final partitioning based on cellColor and faceColor value
	var qPart		= partition(q.cellColor, colors)
	var qParte1		= preimage(q,qPart,q.e1)
	var qParte2		= preimage(q,qPart,q.e2)
	var qParte3		= preimage(q,qPart,q.e3)
	var qPartHalo	= ( qParte1 | qParte2 | qParte3 ) - qPart
	var QMFacePart	= partition(QMFace.faceColor, colors)
	var QPFacePart	= partition(QPFace.faceColor, colors)

	var gridEToVPart	= partition(gridEToV.cellColor, colors)
	var gridVertexPart1	= image(gridVertex, gridEToVPart, gridEToV.v1)
	var gridVertexPart2	= image(gridVertex, gridEToVPart, gridEToV.v2)
	var gridVertexPart3	= image(gridVertex, gridEToVPart, gridEToV.v3)
	var gridVertexPart	= gridVertexPart1 | gridVertexPart2 | gridVertexPart3


	-- 8) Preprocessing and initialize the solution
	for color in colors do
		if ( [int8](color) == 0 ) then
			c.printf("----------Build Internal DOFs Info----------\n\n")
		end
		buildNodes(p_space,gridVertexPart[color],gridEToVPart[color],qPart[color])
	end
	for color in colors do
		if ( [int8](color) == 0 ) then
			c.printf("---Calculate Jacobians And Normal Vectors---\n\n")
		end
		calcGeoFacAndNormal(p_space,nSpaceInt,Dr,Ds,DrSpaceInt,DsSpaceInt,qPart[color])
	end
	for color in colors do
		if ( [int8](color) == 0 ) then
			c.printf("------------Initialize Solution-------------\n\n")
		end
		solutionAtTimeT(0.0,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,config.p0Val,qPart[color])
	end
	__fence(__execution, __block)

	c.printf("------------Run main simulation-------------\n")
	c.printf("simTime = %9.4lf\n",simTime)
	var ts_start = c.legion_get_current_time_in_micros()


	-- 9) Run main simulation
	__demand(__spmd)
	while  simTime < finalTime do
		-- Set values for reduction
		dt = 1.0e+308
		tolSolAderRho = 0.0
		tolSolAderRhouRhov = 0.0
		tolSolAderEner = 0.0

		-- Calculate time step
		for color in colors do
			dt min=	Euler2DDT(Nfp,p_space,CFL,qPart[color])
		end
		--if ( simTime + dt >= finalTime ) then
		--	dt = finalTime - simTime
		--end
		dt = [int](simTime + dt < finalTime) * dt + [int](simTime + dt >= finalTime) * (finalTime - simTime)

		-- Calculate tolerance value for predictor step
		for color in colors do
			tolSolAderRho		max= calcTolSolAderRho(nDOFs,qPart[color])
		end
		for color in colors do
			tolSolAderRhouRhov	max= calcTolSolAderRhouRhov(nDOFs,qPart[color])
		end
		for color in colors do
			tolSolAderEner		max= calcTolSolAderEner(nDOFs,qPart[color])
		end

		-- Predictor step
		for color in colors do
			Euler2DPredictorWrapper(p_space,nDOFs,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpacePart[color],DrSpaceIntPart[color],DsSpaceIntPart[color],wSpaceIntPart[color],DOFToIntSpaceTransposePart[color],lFirstPart[color],wTimeIntPart[color],DOFToIntTimePart[color],AderIterMatPart[color],vmapMPart[color],qPart[color],qPartHalo[color],QMFacePart[color],QPFacePart[color])
		end

		-- Corrector step
		for color in colors do
			Euler2DCorrector(p_space,nDOFs,Nt,nTimeInt,DrwPart[color],DswPart[color],LIFTPart[color],wTimeIntPart[color],DOFToIntTimePart[color],qPart[color],QMFacePart[color],QPFacePart[color])
		end

		-- Update the solution
		for color in colors do
			Euler2DUpdateSolution(dt, nDOFs, qPart[color])
		end

		-- Marching one time step
--		c.printf("simTime = %9.4lf\n",simTime)	-- This line should be commented if SPMD is used
		simTime += dt
	end
	__fence(__execution, __block)
	var ts_stop = c.legion_get_current_time_in_micros()
	c.printf("simTime = %9.4lf\n",simTime)
	c.printf("\nEuler2D simultation completed in %.4f sec\n\n",(ts_stop - ts_start) * 1e-6)


	--10) Calculate error
    for color in colors do
        L1Error     +=  calcL1Error(simTime,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,qPart[color])
    end
    for color in colors do
        L2Error     +=  calcL2Error(simTime,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,qPart[color])
    end
    for color in colors do
        LInfError   max= calcLInfError(simTime,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,qPart[color])
    end

    L1Error /= (nDOFs*gridK)
    L2Error = sqrt(L2Error/(nDOFs*gridK))

    c.printf("----------------Error report----------------\n")
    c.printf("L1   = %12.4e\n",L1Error)
    c.printf("L2   = %12.4e\n",L2Error)
    c.printf("LInf = %12.4e\n",LInfError)
--]]
end

regentlib.start(toplevel)
