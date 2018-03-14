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

-- Read number of faces
function readNFaces(file)
	if not fileExists(file) then
		print("Face Info file",file,"doesn't exits in current directory.")
		print("TERMINATE THE PROGRAM..")
		os.exit()
	end

	local NFaces, line
	local f = io.open(file,"rb")
	line = f:read()
	NFaces = tonumber(line)
	f:close()

	return NFaces
end

local p_space	= findStringVal(configFileName,"p_space")
local p_time	= findStringVal(configFileName,"p_time")

local NpG		= (p_space+1)*(p_space+2)/2
local NtG		= p_time+1
local NfpG		= p_space+1
local NFacesG

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
local faceFileName
if ( gridKG == 166 ) then
	EToEFileName = "EToE009.dat"; EToFFileName = "EToF009.dat";
	partFileName = "grid009Part"; faceFileName = "faceInfo009.dat"
elseif ( gridKG == 640 ) then
	EToEFileName = "EToE017.dat"; EToFFileName = "EToF017.dat";
	partFileName = "grid017Part"; faceFileName = "faceInfo017.dat"
elseif ( gridKG == 2454 ) then
	EToEFileName = "EToE033.dat"; EToFFileName = "EToF033.dat";
	partFileName = "grid033Part"; faceFileName = "faceInfo033.dat"
elseif ( gridKG == 9638 ) then
	EToEFileName = "EToE065.dat"; EToFFileName = "EToF065.dat";
	partFileName = "grid065Part"; faceFileName = "faceInfo065.dat"
elseif ( gridKG == 38428 ) then
	EToEFileName = "EToE129.dat"; EToFFileName = "EToF129.dat";
	partFileName = "grid129Part"; faceFileName = "faceInfo129.dat"
elseif ( gridKG == 163426 ) then
	EToEFileName = "EToE257.dat"; EToFFileName = "EToF257.dat";
	partFileName = "grid257Part"; faceFileName = "faceInfo257.dat"
elseif ( gridKG == 686300 ) then
	EToEFileName = "EToE513.dat"; EToFFileName = "EToF513.dat";
	partFileName = "grid513Part"; faceFileName = "faceInfo513.dat"
elseif ( gridKG == 2821764 ) then
	EToEFileName = "EToE1025.dat"; EToFFileName = "EToF1025.dat";
	partFileName = "grid1025Part"; faceFileName = "faceInfo1025.dat"
end

NFacesG = readNFaces(faceFileName)

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
	timeLev			: int1d;
	factTimeLev		: double;
	factTimeLevRev	: uint64
}

fspace Surface {
	rho			: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	rhou		: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	rhov		: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	ener		: double[3*NfpG][nTimeIntG];	-- nTimeInt*(Nfaces*Nfp)
	faceInd		: uint64;
	faceColor	: int1d;
}

fspace surf {
	elem0			: uint64;
	elem1			: uint64;
	face0			: uint64;
	face1			: uint64;
	isDiffTimeLev	: bool;
	isElem1Halo		: bool;
	faceColorGraph	: int1d;
	faceColorTime	: int1d
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

terra readUintVal(f : &c.FILE, val : &uint64)
	return c.fscanf(f,"%llu\n",&val[0]) == 1
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
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			MSpace[e].v = val[0]
		end

		-- Dr
		for e in Dr do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			Dr[e].v = val[0]
		end

		-- Ds
		for e in Ds do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			Ds[e].v = val[0]
		end

		-- Drw
		for e in Drw do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			Drw[e].v = val[0]
		end

		-- Dsw
		for e in Dsw do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			Dsw[e].v = val[0]
		end

		-- LIFT
		for e in LIFT do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			LIFT[e].v = val[0]
		end

		-- DrSpaceInt
		for e in DrSpaceInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			DrSpaceInt[e].v = val[0]
		end

		-- DsSpaceInt
		for e in DsSpaceInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			DsSpaceInt[e].v = val[0]
		end

		-- wSpaceInt
		for e in wSpaceInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
			wSpaceInt[e].v = val[0]
		end

		-- DOFToIntSpaceTranspose
		for e in DOFToIntSpaceTranspose do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementSpaceInfo file")
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

task readStdElemTimeInfo(fileName : &int8, lFirst : region(ispace(int1d),doubleVal), wTimeInt : region(ispace(int1d),doubleVal), DOFToIntTime : region(ispace(int1d),doubleVal), DOFToIntAdjTime : region(ispace(int1d),doubleVal))
where
	reads writes(lFirst.v, wTimeInt.v, DOFToIntTime.v, DOFToIntAdjTime.v) 
do
	if isFile(fileName) then
		c.printf("-----------Read Std.Elem Time File----------\n\n")
		var f = c.fopen(fileName,"r")
		var val : double[1]

		-- lFirst
		for e in lFirst do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementTimeInfo file")
			lFirst[e].v = val[0]
		end

		-- wTimeInt
		for e in wTimeInt do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementTimeInfo file")
			wTimeInt[e].v = val[0]
		end

		-- DOFToIntTime
		for e in DOFToIntTime do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementTimeInfo file")
			DOFToIntTime[e].v = val[0]
		end

		-- DOFToIntAdjTime
		for e in DOFToIntAdjTime do
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in standardElementTimeInfo file")
			DOFToIntAdjTime[e].v = val[0]
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
			regentlib.assert(readDoubleVal(f,val), "Less data that it should be in aderIterMat file")
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
		regentlib.assert(readCoord(f,coord),"Error in readVertex task. Less coordinates data than it should be. Check grid file.")
	end

	-- Read coordinates
	for e in gridVertex do
		regentlib.assert(readCoord(f,coord),"Error in readVertex task. Less coordinates data than it should be. Check grid file.")
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
		regentlib.assert(readCoord(f,coord),"Error in readElemToVertex task. Less coordinates data than it should be. Check grid file.")
	end

	-- Reads data until corresponding partition
	for ii=0, lowerBound do
		regentlib.assert(readEToV(f,EToVVal),"Error in readElemToVertex task. Less EToV data than it should be. Check grid file.")
	end

	-- Read EToV info
	for e in gridEToV do
		regentlib.assert(readEToV(f,EToVVal),"Error in readElemToVertex task. Less EToV data than it should be. Check grid file.")
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

task buildNodes(p_space : int8, nTimeLev : uint64, gridVertex : region(GridVertex), gridEToV : region(GridEToV(gridVertex)), q : region(ispace(ptr), Elem))
where
	reads(q._x,q._y,q.timeLev,q.factTimeLev,q.factTimeLevRev,q.cellInd,gridVertex.VX,gridVertex.VY,gridEToV.v1,gridEToV.v2, gridEToV.v3),
	writes(q._x,q._y,q.timeLev,q.factTimeLev,q.factTimeLevRev)
do
	var dh			: double = 2.0/p_space
	var upBound		: uint64
	var rDum		: double
	var sDum		: double
	var xCenter		: double
	var yCenter		: double

	var cellNum : uint64
	var cnt : uint64
	for e in q do
		cellNum = e.cellInd
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

		if ( nTimeLev == 1 ) then
			e.timeLev = [int1d](0)
			e.factTimeLev = 1.0
			e.factTimeLevRev = 1
		else
			-- Hardcoded time levels for LTS
			xCenter = (gridVertex[gridEToV[cellNum].v1].VX + gridVertex[gridEToV[cellNum].v2].VX + gridVertex[gridEToV[cellNum].v3].VX)/3
			yCenter = (gridVertex[gridEToV[cellNum].v1].VY + gridVertex[gridEToV[cellNum].v2].VY + gridVertex[gridEToV[cellNum].v3].VY)/3
			if ( nTimeLev == 2 ) then
				if ( pow(xCenter-5.0,2) + pow(yCenter-0.0,2) - 1.5*1.5 < 0 ) then
					e.timeLev = [int1d](0)
					e.factTimeLev = pow(2.0,1.0) -- 2^(nTimeLev-1-curTimeLev)
					e.factTimeLevRev = pow(2,0)
				else
					e.timeLev = [int1d](1)
					e.factTimeLev = pow(2.0,0.0) -- 2^(nTimeLev-1-curTimeLev)
					e.factTimeLevRev = pow(2,1)
				end
			elseif ( nTimeLev == 3 ) then
				if ( pow(xCenter-5.0,2) + pow(yCenter-0.0,2) - 0.5*0.5 < 0 ) then
					e.timeLev = [int1d](0)
					e.factTimeLev = pow(2.0,2.0) -- 2^(nTimeLev-1-curTimeLev)
					e.factTimeLevRev = pow(2,0)
				elseif ( pow(xCenter-5.0,2) + pow(yCenter-0.0,2) - 1.5*1.5 < 0 ) then
					e.timeLev = [int1d](1)
					e.factTimeLev = pow(2.0,1.0) -- 2^(nTimeLev-1-curTimeLev)
					e.factTimeLevRev = pow(2,1)
				else
					e.timeLev = [int1d](2)
					e.factTimeLev = pow(2.0,0.0) -- 2^(nTimeLev-1-curTimeLev)
					e.factTimeLevRev = pow(2,2)
				end
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
		cellNum = e.cellInd
	
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

terra readFace(f : &c.FILE, faceInfoVal : &uint64)
	return c.fscanf(f,"%llu %llu %llu %llu\n",&faceInfoVal[0],&faceInfoVal[1],&faceInfoVal[2],&faceInfoVal[3]) == 4
end

task readFaceInfo(faceFileName : &int8, face : region(ispace(ptr),surf))
where
	reads writes(face.elem0, face.elem1, face.face0, face.face1, face.isDiffTimeLev, face.isElem1Halo, face.faceColorGraph, face.faceColorTime)
do
	var faceInfo	: uint64[4]
	var dummyVal	: uint64[1]
	var lowerBound  : uint64
	for e in face do
		lowerBound = [uint64](e)
		break
	end

	var f = c.fopen(faceFileName,"r")
	readUintVal(f,dummyVal)
	for ii=0,lowerBound do
		regentlib.assert(readFace(f,faceInfo),"Error in partitioning info. Check partitioning data file.")
	end
	for e in face do
		regentlib.assert(readFace(f,faceInfo),"Error in partitioning info. Check partitioning data file.")
		e.elem0			= faceInfo[0]
		e.elem1			= faceInfo[1]
		e.face0			= faceInfo[2]
		e.face1			= faceInfo[3]
		e.isDiffTimeLev	= false
		e.isElem1Halo	= false
		e.faceColorGraph= [int1d](0)
		e.faceColorTime	= [int1d](0)
	end
	c.fclose(f)
end

task colorFaces(q : region(ispace(ptr), Elem), faceEqual : region(ispace(ptr), surf))
where
	reads(q.timeLev, q.cellColor, faceEqual.elem0, faceEqual.elem1, faceEqual.face0, faceEqual.face1, faceEqual.faceColorGraph, faceEqual.faceColorTime, faceEqual.isDiffTimeLev, faceEqual.isElem1Halo),
	writes(faceEqual.elem0, faceEqual.elem1, faceEqual.face0, faceEqual.face1, faceEqual.faceColorGraph,faceEqual.faceColorTime,faceEqual.isDiffTimeLev, faceEqual.isElem1Halo)
do
	var cellInd		: uint64
	var colorVal	: uint64
	var lowerBound	: uint64
	var timeLev0	: uint64
	var timeLev1	: uint64
	var sumElemID	: uint64
	var elemDum		: uint64
	var faceDum		: uint64

	for e in faceEqual do
		lowerBound = [uint64](e)
		break
	end

	for e in faceEqual do
		-- Coloring criteria for faces
		-- 1) A face belongs to a cell which has lower time level
		-- 2) If both elements are same time level, try to distribute evenly
		timeLev0 = [uint64](q[e.elem0].timeLev)	
		timeLev1 = [uint64](q[e.elem1].timeLev)

		-- Check if both elements have the same time level
		if ( timeLev0 == timeLev1 ) then
			sumElemID = e.elem0 + e.elem1
			if ( sumElemID - (sumElemID/2)*2 == 0 ) then
				e.faceColorGraph= q[e.elem0].cellColor
			else
				e.faceColorGraph= q[e.elem1].cellColor

				-- Swap the face info in order to make
				-- elem0 is the owner of current face
				elemDum = e.elem0
				faceDum = e.face0

				e.elem0 = e.elem1
				e.elem1 = elemDum
				if ( e.face1 > 9 ) then
					e.face0 = e.face1/10
					e.face1 = faceDum*10
				else
					e.face0 = e.face1
					e.face1 = faceDum
				end
			end
			e.faceColorTime = [int1d](timeLev0)
			if not ( [uint64](q[e.elem0].cellColor) == [uint64](q[e.elem1].cellColor) ) then
				e.isElem1Halo = true
			end
		else
			-- The time level of both elements differ
			-- Elements with smaller time level will own this face
			if ( timeLev0 < timeLev1 ) then
				e.faceColorGraph= q[e.elem0].cellColor
				e.faceColorTime	= [int1d](timeLev0)
			else
				e.faceColorGraph= q[e.elem1].cellColor
				e.faceColorTime	= [int1d](timeLev1)

				-- Swap the face info in order to make
				-- elem0 is the owner of current face
				elemDum = e.elem0
				faceDum = e.face0

				e.elem0 = e.elem1
				e.elem1 = elemDum
				if ( e.face1 > 9 ) then
					e.face0 = e.face1/10
					e.face1 = faceDum*10
				else
					e.face0 = e.face1
					e.face1 = faceDum
				end
			end
			if not ( [uint64](q[e.elem0].cellColor) == [uint64](q[e.elem1].cellColor) ) then
				e.isElem1Halo = true
			end
			e.isDiffTimeLev = true
		end
	end
end

task solutionAtTimeT(time : double, Np : uint64, epsVal : double, rho0Val : double, u0Val : double, v0Val : double, p0Val : double, q : region(ispace(ptr), Elem))
where
	reads (q._x, q._y, q.sol, q.volRes, q.surfRes,q.cellInd),
	writes(q.sol, q.volRes, q.surfRes)
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
		cellNum = e.cellInd

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

			-- Initialize volume and surface residuals to zero
            q[cellNum].volRes[jj].rho = 0.0
            q[cellNum].volRes[jj].rhou = 0.0
            q[cellNum].volRes[jj].rhov = 0.0
            q[cellNum].volRes[jj].ener = 0.0
            q[cellNum].surfRes[jj].rho = 0.0
            q[cellNum].surfRes[jj].rhou = 0.0
            q[cellNum].surfRes[jj].rhov = 0.0
            q[cellNum].surfRes[jj].ener = 0.0
		end
	end
end

task Euler2DDT(Nfp : int8, p_space : int8, CFL : double, q : region(ispace(ptr), Elem))
where
	reads(q.sol, q.Fscale, q.cellInd, q.factTimeLev)
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
        cellNum = e.cellInd

		-- Face 1
		cnt = 0
		for jj=0, Nfp do
			curDOF = jj
			u = q[cellNum].sol[curDOF].rhou/q[cellNum].sol[curDOF].rho
			v = q[cellNum].sol[curDOF].rhov/q[cellNum].sol[curDOF].rho
			p = (gamma-1.0)*( q[cellNum].sol[curDOF].ener - q[cellNum].sol[curDOF].rho*(u*u + v*v)/2.0 )
			a = sqrt(abs(gamma*p/q[cellNum].sol[curDOF].rho))

			dt = 1.0/( pow((p_space+1.0),2.0)*0.5*q[cellNum].Fscale[cnt]*(sqrt(u*u + v*v) + a) )*q[cellNum].factTimeLev
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

			dt = 1.0/( pow((p_space+1.0),2.0)*0.5*q[cellNum].Fscale[cnt]*(sqrt(u*u + v*v) + a) )*q[cellNum].factTimeLev
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

			dt = 1.0/( pow((p_space+1.0),2.0)*0.5*q[cellNum].Fscale[cnt]*(sqrt(u*u + v*v) + a) )*q[cellNum].factTimeLev
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
		cellNum = e.cellInd
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
		cellNum = e.cellInd
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
		cellNum = e.cellInd
		for jj=0,Np do
            maxEner = max(maxEner,q[cellNum].sol[jj].ener*thres)
		end
	end

	return maxEner
end

__demand(__inline)
task Euler2DPredictor(cellNum : uint64, Np : uint64, Nt : uint64, nSpaceInt : uint64, nTimeInt : uint64, dt : double, tolSolAderRho : double, tolSolAderRhouRhov : double, tolSolAderEner : double, MSpace : region(ispace(int1d), doubleVal), DrSpaceInt : region(ispace(int1d), doubleVal), DsSpaceInt : region(ispace(int1d), doubleVal), wSpaceInt : region(ispace(int1d), doubleVal), DOFToIntSpaceTranspose : region(ispace(int1d), doubleVal), lFirst : region(ispace(int1d), doubleVal), wTimeInt : region(ispace(int1d), doubleVal), DOFToIntTime : region(ispace(int1d), doubleVal), AderIterMat : region(ispace(int1d),doubleVal), q : region(ispace(ptr), Elem), preSolRho : &double, preSolRhou : &double, preSolRhov : &double, preSolEner : &double)
where
    reads(MSpace.v, DrSpaceInt.v, DsSpaceInt.v, wSpaceInt.v, DOFToIntSpaceTranspose.v, lFirst.v, wTimeInt.v, DOFToIntTime.v, AderIterMat.v, q.sol, q.rxInt, q.sxInt, q.ryInt, q.syInt, q.factTimeLev)
do
	var NtNp			: uint64 = Nt*Np
	var cntTot 			: uint64 = 0
	var indVal			: uint64
	var p				: double
	var u				: double
	var v				: double
	var rH				: double
	var gamma			: double = 1.4
	var w				: double
	var fluxF1			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np, Ps=9 
	var fluxF2			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var fluxF3			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var fluxF4			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var fluxG1			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var fluxG2			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var fluxG3			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var fluxG4			: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var gradFluxesIntFr1: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt, Ps=9 
	var gradFluxesIntFr2: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntFr3: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntFr4: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntFs1: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntFs2: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntFs3: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntFs4: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGr1: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGr2: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGr3: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGr4: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGs1: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGs2: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGs3: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var gradFluxesIntGs4: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt
	var resSolRho		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt), Ps=9, Pt=3
	var resSolRhou		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var resSolRhov		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var resSolEner		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var oldSolRho		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var oldSolRhou		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var oldSolRhov		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var oldSolEner		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var intSolRho		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var intSolRhou		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var intSolRhov		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var intSolEner		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var resIntRho		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var resIntRhou		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var resIntRhov		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var resIntEner		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var resDumRho		: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt 
	var resDumRhou		: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt 
	var resDumRhov		: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt 
	var resDumEner		: &double = [&double](c.malloc(nSpaceInt*[terralib.sizeof(double)]))		-- nSpaceInt 
	var resTotRho		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var resTotRhou		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var resTotRhov		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var resTotEner		: &double = [&double](c.malloc(NtNp*[terralib.sizeof(double)]))		-- (Np*Nt)
	var isConverged		: bool = true

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
				w = 0.5*wTimeInt[jj].v*(dt/q[cellNum].factTimeLev)*DOFToIntTime[jj*Nt+kk].v
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
	if ( cntTot > 90 ) then
		c.printf("Euler2DPredictor step is failed in cellNum = %10llu\n",cellNum)
		c.printf("TERMINATE THE PROGRAM..")
		c.abort()
	end

	-- Free buffers
	c.free(fluxF1)
	c.free(fluxF2)
	c.free(fluxF3)
	c.free(fluxF4)
	c.free(fluxG1)
	c.free(fluxG2)
	c.free(fluxG3)
	c.free(fluxG4)
	c.free(gradFluxesIntFr1)
	c.free(gradFluxesIntFr2)
	c.free(gradFluxesIntFr3)
	c.free(gradFluxesIntFr4)
	c.free(gradFluxesIntFs1)
	c.free(gradFluxesIntFs2)
	c.free(gradFluxesIntFs3)
	c.free(gradFluxesIntFs4)
	c.free(gradFluxesIntGr1)
	c.free(gradFluxesIntGr2)
	c.free(gradFluxesIntGr3)
	c.free(gradFluxesIntGr4)
	c.free(gradFluxesIntGs1)
	c.free(gradFluxesIntGs2)
	c.free(gradFluxesIntGs3)
	c.free(gradFluxesIntGs4)
	c.free(resSolRho)
	c.free(resSolRhou)
	c.free(resSolRhov)
	c.free(resSolEner)
	c.free(oldSolRho)
	c.free(oldSolRhou)
	c.free(oldSolRhov)
	c.free(oldSolEner)
	c.free(intSolRho)
	c.free(intSolRhou)
	c.free(intSolRhov)
	c.free(intSolEner)
	c.free(resIntRho)
	c.free(resIntRhou)
	c.free(resIntRhov)
	c.free(resIntEner)
	c.free(resDumRho)
	c.free(resDumRhou)
	c.free(resDumRhov)
	c.free(resDumEner)
	c.free(resTotRho)
	c.free(resTotRhou)
	c.free(resTotRhov)
	c.free(resTotEner)
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
task Euler2DLF(cellInd : uint64, p_space : int8, lB : uint64, F1 : &double, F2 : &double, F3 : &double, F4 : &double, nx : &double, ny : &double, QMRho : &double, QMRhou : &double, QMRhov : &double, QMEner : &double, QPRho : &double, QPRhou : &double, QPRhov : &double, QPEner : &double)
	var maxVelCand : double
	var maxVelTemp : double
	var gamma	: double = 1.4
	var Nfp		: uint64 = p_space+1 
	var maxVel	: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp, Ps=9
	var rhoM	: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var rhouM	: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var rhovM	: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var enerM	: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var uM		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var vM		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var pM		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var rhoP    : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var rhouP   : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var rhovP   : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var enerP   : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var uP		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var vP		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
	var pP		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp

	var F1M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F2M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F3M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F4M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G1M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G2M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G3M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G4M		: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F1P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F2P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F3P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var F4P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G1P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G2P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G3P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp
    var G4P     : &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))	-- Nfaces*Nfp

	for ii=0,Nfp do
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
	Euler2DFluxes(Nfp,F1M,F2M,F3M,F4M,G1M,G2M,G3M,G4M,rhoM,rhouM,rhovM,enerM)
	Euler2DFluxes(Nfp,F1P,F2P,F3P,F4P,G1P,G2P,G3P,G4P,rhoP,rhouP,rhovP,enerP)

	-- Compute wave speed for Lax-Friedrichs(Rusanov) numerical fluxes
	maxVelCand = 0.0
	for jj=0,Nfp do
		maxVelTemp = max( (sqrt(uM[jj]*uM[jj]+vM[jj]*vM[jj])+sqrt(abs(gamma*pM[jj]/rhoM[jj]))) , (sqrt(uP[jj]*uP[jj]+vP[jj]*vP[jj])+sqrt(abs(gamma*pP[jj]/rhoP[jj]))) )
		maxVelCand = max(maxVelCand,maxVelTemp)
	end
	for jj=0,Nfp do
		maxVel[jj] = maxVelCand
	end

	-- Form LF fluxes
	for ii=0,Nfp do
		F1[ii] = 0.5*( nx[lB+ii]*(F1P[ii]+F1M[ii]) + ny[lB+ii]*(G1P[ii]+G1M[ii]) + maxVel[ii]*(rhoM[ii]-rhoP[ii]))
		F2[ii] = 0.5*( nx[lB+ii]*(F2P[ii]+F2M[ii]) + ny[lB+ii]*(G2P[ii]+G2M[ii]) + maxVel[ii]*(rhouM[ii]-rhouP[ii]))
		F3[ii] = 0.5*( nx[lB+ii]*(F3P[ii]+F3M[ii]) + ny[lB+ii]*(G3P[ii]+G3M[ii]) + maxVel[ii]*(rhovM[ii]-rhovP[ii]))
		F4[ii] = 0.5*( nx[lB+ii]*(F4P[ii]+F4M[ii]) + ny[lB+ii]*(G4P[ii]+G4M[ii]) + maxVel[ii]*(enerM[ii]-enerP[ii]))
	end

	-- Free buffers
	c.free(maxVel)
	c.free(rhoM)
	c.free(rhouM)
	c.free(rhovM)
	c.free(enerM)
	c.free(uM)
	c.free(vM)
	c.free(pM)
    c.free(rhoP)
    c.free(rhouP)
    c.free(rhovP)
    c.free(enerP)
	c.free(uP)
	c.free(vP)
	c.free(pP)
	c.free(F1M)
    c.free(F2M)
    c.free(F3M)
    c.free(F4M)
    c.free(G1M)
    c.free(G2M)
    c.free(G3M)
    c.free(G4M)
    c.free(F1P)
    c.free(F2P)
    c.free(F3P)
    c.free(F4P)
    c.free(G1P)
    c.free(G2P)
    c.free(G3P)
    c.free(G4P)
end

task Euler2DPredictorWrapper(ltsLevel : uint8, Np : uint64, Nt : uint64, nSpaceInt : uint64, nTimeInt : uint64, dt : double, tolSolAderRho : double, tolSolAderRhouRhov : double, tolSolAderEner : double, MSpace : region(ispace(int1d), doubleVal), DrSpaceInt : region(ispace(int1d), doubleVal), DsSpaceInt : region(ispace(int1d), doubleVal), wSpaceInt : region(ispace(int1d), doubleVal), DOFToIntSpaceTranspose : region(ispace(int1d), doubleVal), lFirst : region(ispace(int1d), doubleVal), wTimeInt : region(ispace(int1d), doubleVal), DOFToIntTime : region(ispace(int1d), doubleVal), AderIterMat : region(ispace(int1d),doubleVal), q : region(ispace(ptr), Elem)) 
where
    reads(MSpace.v, DrSpaceInt.v, DsSpaceInt.v, wSpaceInt.v, DOFToIntSpaceTranspose.v, lFirst.v, wTimeInt.v, DOFToIntTime.v, AderIterMat.v, q.sol, q.preSol, q.cellInd, q.rxInt, q.sxInt, q.ryInt, q.syInt, q.factTimeLev, q.factTimeLevRev),
	writes(q.preSol) 
do
	var cellNum			: uint64
	var preSolRho		: &double = [&double](c.malloc(Nt*Np*[terralib.sizeof(double)]))		-- Np*Nt, Ps=9, Pt=3
	var preSolRhou		: &double = [&double](c.malloc(Nt*Np*[terralib.sizeof(double)]))		-- Np*Nt
	var preSolRhov		: &double = [&double](c.malloc(Nt*Np*[terralib.sizeof(double)]))		-- Np*Nt
	var preSolEner		: &double = [&double](c.malloc(Nt*Np*[terralib.sizeof(double)]))		-- Np*Nt
	var elem0Cont		: int8

	-- Note that predictor step occurs
	-- 1) Always for timeLev == 0
	-- 2) mod(ltsLevel,2^timeLev) == 0
	for e in q do
		cellNum	= e.cellInd
		elem0Cont = (ltsLevel/q[cellNum].factTimeLevRev)*q[cellNum].factTimeLevRev-ltsLevel
		if ( elem0Cont == 0 ) then
			-- Solve predictor solution for this element 
			Euler2DPredictor(cellNum,Np,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpace,DrSpaceInt,DsSpaceInt,wSpaceInt,DOFToIntSpaceTranspose,lFirst,wTimeInt,DOFToIntTime,AderIterMat,q,preSolRho,preSolRhou,preSolRhov,preSolEner)

			-- Save predictor solutions
			for ii=0,Np*Nt do
				q[cellNum].preSol[ii].rho  = preSolRho[ii] 
				q[cellNum].preSol[ii].rhou = preSolRhou[ii]
				q[cellNum].preSol[ii].rhov = preSolRhov[ii]
				q[cellNum].preSol[ii].ener = preSolEner[ii]
			end
		end
	end

	-- Free buffers
	c.free(preSolRho)
	c.free(preSolRhou)
	c.free(preSolRhov)
	c.free(preSolEner)
end

task residualSurfaceOwned(ltsLevel : uint8, p_space : uint64, Np : uint64, Nt : uint64, nTimeInt : uint64, wTimeInt : region(ispace(int1d),doubleVal), DOFToIntTime : region(ispace(int1d),doubleVal), DOFToIntAdjTime : region(ispace(int1d),doubleVal), LIFT : region(ispace(int1d),doubleVal), vmapM : region(ispace(int1d),uintVal), q : region(ispace(ptr),Elem), face : region(ispace(ptr),surf) )
where
	reads(wTimeInt.v, DOFToIntTime.v, DOFToIntAdjTime.v, LIFT.v, vmapM.v, q.preSol, q.nx, q.ny, q.Fscale, q.surfRes, q.factTimeLevRev, face.elem0, face.elem1, face.face0, face.face1, face.isElem1Halo, face.isDiffTimeLev),
	writes(q.surfRes) 
do
	var indVal			: uint64
	var indValGrad		: int8
	var isFlipped		: bool
	var lB0				: uint64
	var uB0				: uint64
	var lB1				: uint64
	var uB1				: uint64
	var Nfp				: uint64 = p_space+1
	var NfpSum			: uint64 = 3*Nfp
	var nTimeIntNt		: uint64 = nTimeInt*Nt
	var solIntRho0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np, Ps=9
	var solIntRhou0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRhov0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntEner0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRho1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRhou1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRhov1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntEner1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var QMRho			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp, Ps=9
	var QMRhou			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QMRhov			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QMEner			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPRho			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPRhou			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPRhov			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPEner			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var F1s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp, Ps=9
	var F2s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var F3s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var F4s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var halfWeight		: double
	var quartWeight		: double
	var tempValRho		: double
	var tempValRhou		: double
	var tempValRhov		: double
	var tempValEner		: double
	var cellNum			: uint64
	var elem0Cont		: int8
	var elem1Cont		: int8
	var isSecond		: int8


	for e in face do
		if not ( e.isElem1Halo ) then
			-- Elem0 and Elem1 are in the same graph partition
			-- Now we may have two cases where
			-- 1) Both elements are same time level or
			-- 2) Elem1 has higher time level
			-- For both cases, we need to calculate surface 
			-- residual contribution of current face based on
			-- ltsLevel and factTimeLev value.
			-- For example, for three time level case
			--                   ltsLevel
			--             0      1      2      3
			-- ------------------------------------
			-- timeLev0    O      O      O      O
			-- timeLev1    X      O      X      O
			-- timeLev2    X      X      X      O

			elem0Cont = ((ltsLevel+1)/q[e.elem0].factTimeLevRev)*q[e.elem0].factTimeLevRev-(ltsLevel+1)
			if ( elem0Cont == 0 ) then
				elem1Cont = ((ltsLevel+1)/q[e.elem1].factTimeLevRev)*q[e.elem1].factTimeLevRev-(ltsLevel+1)

				-- 1) Both elements are same time level
				if not ( e.isDiffTimeLev ) then
					cellNum = e.elem0

					-- Determine face orientation
					lB0 = (e.face0-1)*Nfp
					uB0 = e.face0*Nfp
					isFlipped = false
					indValGrad = 1
					if ( e.face1 > 9 ) then
						-- Elem1 side is flipped
						lB1 = (e.face1/10-1)*Nfp
						uB1 = e.face1*Nfp/10
						isFlipped = true
					else
						lB1 = (e.face1-1)*Nfp
						uB1 = e.face1*Nfp
					end

					for ii=0,nTimeInt do
						-- Temporal weight for surface residual calculation
						halfWeight = 0.5*wTimeInt[ii].v

						-- Interpolate predictor solution of elem0
						for kk=0,Np do
							solIntRho0[kk]	= 0.0
							solIntRhou0[kk]	= 0.0
							solIntRhov0[kk]	= 0.0
							solIntEner0[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rho
								solIntRhou0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhou
								solIntRhov0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhov
								solIntEner0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QM values
						indVal = 0
						for kk=lB0,uB0 do
							QMRho[indVal]	= solIntRho0[vmapM[kk].v]
							QMRhou[indVal]	= solIntRhou0[vmapM[kk].v]
							QMRhov[indVal]	= solIntRhov0[vmapM[kk].v]
							QMEner[indVal]	= solIntEner0[vmapM[kk].v]
							indVal = indVal + 1
						end
        
						-- Interpolate predictor solution of elem1
						for kk=0,Np do
							solIntRho1[kk]	= 0.0
							solIntRhou1[kk]	= 0.0
							solIntRhov1[kk]	= 0.0
							solIntEner1[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].rho
								solIntRhou1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].rhou
								solIntRhov1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].rhov
								solIntEner1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QP values 
						indVal = 0
						indValGrad = 1
						if ( isFlipped ) then
							indVal = Nfp-1
							indValGrad = -1						
						end
    
						for kk=lB1,uB1 do
							QPRho[indVal]	= solIntRho1[vmapM[kk].v]
							QPRhou[indVal]	= solIntRhou1[vmapM[kk].v]
							QPRhov[indVal]	= solIntRhov1[vmapM[kk].v]
							QPEner[indVal]	= solIntEner1[vmapM[kk].v]
							indVal = indVal + indValGrad
						end
        
						-- Calculate surface fluxes
						Euler2DLF(cellNum, p_space, lB0, F1s, F2s, F3s, F4s, q[cellNum].nx, q[cellNum].ny, QMRho, QMRhou, QMRhov, QMEner, QPRho, QPRhou, QPRhov, QPEner)

						-- Update elem0 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							for ll=lB0,uB0 do
								tempValRho	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F1s[indVal]
								tempValRhou	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F2s[indVal]
								tempValRhov	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F3s[indVal]
								tempValEner	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F4s[indVal]
								indVal = indVal + 1 
							end
            
							q[e.elem0].surfRes[kk].rho	+= (halfWeight*tempValRho)
							q[e.elem0].surfRes[kk].rhou	+= (halfWeight*tempValRhou)
							q[e.elem0].surfRes[kk].rhov	+= (halfWeight*tempValRhov)
							q[e.elem0].surfRes[kk].ener	+= (halfWeight*tempValEner)
						end

						-- Update elem1 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							indValGrad = 1
							if ( isFlipped ) then
								indVal = Nfp-1
								indValGrad = -1						
							end
							for ll=lB1,uB1 do
								-- Note that signs are flipped compared to elem0
								tempValRho	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F1s[indVal]
								tempValRhou	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F2s[indVal]
								tempValRhov	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F3s[indVal]
								tempValEner	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F4s[indVal]
								indVal = indVal + indValGrad
							end
							q[e.elem1].surfRes[kk].rho	+= (halfWeight*tempValRho)
							q[e.elem1].surfRes[kk].rhou	+= (halfWeight*tempValRhou)
							q[e.elem1].surfRes[kk].rhov	+= (halfWeight*tempValRhov)
							q[e.elem1].surfRes[kk].ener	+= (halfWeight*tempValEner)
						end
					end
				-- 2) Elem1 has higher time level
				else
					cellNum = e.elem0
					if ( elem1Cont == 0 ) then
						isSecond = 1
					else
						isSecond = 0
					end

					-- Determine face orientation
					lB0 = (e.face0-1)*Nfp
					uB0 = e.face0*Nfp
					isFlipped = false
					indValGrad = 1
					if ( e.face1 > 9 ) then
						-- Elem1 side is flipped
						lB1 = (e.face1/10-1)*Nfp
						uB1 = e.face1*Nfp/10
						isFlipped = true
					else
						lB1 = (e.face1-1)*Nfp
						uB1 = e.face1*Nfp
					end

					for ii=0,nTimeInt do
						-- Temporal weight for surface residual calculation
						halfWeight = 0.5*wTimeInt[ii].v
						quartWeight = 0.25*wTimeInt[ii].v

						-- Interpolate predictor solution of elem0
						for kk=0,Np do
							solIntRho0[kk]	= 0.0
							solIntRhou0[kk]	= 0.0
							solIntRhov0[kk]	= 0.0
							solIntEner0[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rho
								solIntRhou0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhou
								solIntRhov0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhov
								solIntEner0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QM values
						indVal = 0
						for kk=lB0,uB0 do
							QMRho[indVal]	= solIntRho0[vmapM[kk].v]
							QMRhou[indVal]	= solIntRhou0[vmapM[kk].v]
							QMRhov[indVal]	= solIntRhov0[vmapM[kk].v]
							QMEner[indVal]	= solIntEner0[vmapM[kk].v]
							indVal = indVal + 1
						end
        
						-- Interpolate predictor solution of elem1
						for kk=0,Np do
							solIntRho1[kk]	= 0.0
							solIntRhou1[kk]	= 0.0
							solIntRhov1[kk]	= 0.0
							solIntEner1[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].rho
								solIntRhou1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].rhou
								solIntRhov1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].rhov
								solIntEner1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QP values 
						indVal = 0
						indValGrad = 1
						if ( isFlipped ) then
							indVal = Nfp-1
							indValGrad = -1						
						end
    
						for kk=lB1,uB1 do
							QPRho[indVal]	= solIntRho1[vmapM[kk].v]
							QPRhou[indVal]	= solIntRhou1[vmapM[kk].v]
							QPRhov[indVal]	= solIntRhov1[vmapM[kk].v]
							QPEner[indVal]	= solIntEner1[vmapM[kk].v]
							indVal = indVal + indValGrad
						end
        
						-- Calculate surface fluxes
						Euler2DLF(cellNum, p_space, lB0, F1s, F2s, F3s, F4s, q[cellNum].nx, q[cellNum].ny, QMRho, QMRhou, QMRhov, QMEner, QPRho, QPRhou, QPRhov, QPEner)

						-- Update elem0 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							for ll=lB0,uB0 do
								tempValRho	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F1s[indVal]
								tempValRhou	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F2s[indVal]
								tempValRhov	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F3s[indVal]
								tempValEner	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F4s[indVal]
								indVal = indVal + 1 
							end
            
							q[e.elem0].surfRes[kk].rho	+= (halfWeight*tempValRho)
							q[e.elem0].surfRes[kk].rhou	+= (halfWeight*tempValRhou)
							q[e.elem0].surfRes[kk].rhov	+= (halfWeight*tempValRhov)
							q[e.elem0].surfRes[kk].ener	+= (halfWeight*tempValEner)
						end

						-- Update elem1 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							indValGrad = 1
							if ( isFlipped ) then
								indVal = Nfp-1
								indValGrad = -1						
							end
							for ll=lB1,uB1 do
								-- Note that signs are flipped compared to elem0
								tempValRho	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F1s[indVal]
								tempValRhou	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F2s[indVal]
								tempValRhov	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F3s[indVal]
								tempValEner	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F4s[indVal]
								indVal = indVal + indValGrad
							end
							q[e.elem1].surfRes[kk].rho	+= (quartWeight*tempValRho)
							q[e.elem1].surfRes[kk].rhou	+= (quartWeight*tempValRhou)
							q[e.elem1].surfRes[kk].rhov	+= (quartWeight*tempValRhov)
							q[e.elem1].surfRes[kk].ener	+= (quartWeight*tempValEner)
						end
					end
				end
			end
		end
	end

	-- Free buffers
	c.free(solIntRho0)
	c.free(solIntRhou0)
	c.free(solIntRhov0)
	c.free(solIntEner0)
	c.free(solIntRho1)
	c.free(solIntRhou1)
	c.free(solIntRhov1)
	c.free(solIntEner1)
	c.free(QMRho)
	c.free(QMRhou)
	c.free(QMRhov)
	c.free(QMEner)
	c.free(QPRho)
	c.free(QPRhou)
	c.free(QPRhov)
	c.free(QPEner)
	c.free(F1s)
	c.free(F2s)
	c.free(F3s)
	c.free(F4s)
end

task residualSurfaceHalo(ltsLevel : uint8, p_space : uint64, Np : uint64, Nt : uint64, nTimeInt : uint64, wTimeInt : region(ispace(int1d),doubleVal), DOFToIntTime : region(ispace(int1d),doubleVal), DOFToIntAdjTime : region(ispace(int1d),doubleVal), LIFT : region(ispace(int1d),doubleVal), vmapM : region(ispace(int1d),uintVal), q : region(ispace(ptr),Elem), face : region(ispace(ptr),surf) )
where
	reads(wTimeInt.v, DOFToIntTime.v, DOFToIntAdjTime.v, LIFT.v, vmapM.v, q.preSol, q.nx, q.ny, q.Fscale, q.surfRes, q.factTimeLevRev, face.elem0, face.elem1, face.face0, face.face1, face.isElem1Halo, face.isDiffTimeLev),
	reduces+(q.surfRes)
do
	var indVal			: uint64
	var indValGrad		: int8
	var isFlipped		: bool
	var lB0				: uint64
	var uB0				: uint64
	var lB1				: uint64
	var uB1				: uint64
	var Nfp				: uint64 = p_space+1
	var NfpSum			: uint64 = 3*Nfp
	var nTimeIntNt		: uint64 = nTimeInt*Nt
	var solIntRho0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np, Ps=9
	var solIntRhou0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRhov0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntEner0		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRho1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRhou1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntRhov1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var solIntEner1		: &double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var QMRho			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp, Ps=9
	var QMRhou			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QMRhov			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QMEner			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPRho			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPRhou			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPRhov			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var QPEner			: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var F1s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp, Ps=9
	var F2s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var F3s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var F4s				: &double = [&double](c.malloc(Nfp*[terralib.sizeof(double)]))		-- Nfp
	var halfWeight		: double
	var quartWeight		: double
	var tempValRho		: double
	var tempValRhou		: double
	var tempValRhov		: double
	var tempValEner		: double
	var cellNum			: uint64
	var elem0Cont		: int8
	var elem1Cont		: int8
	var isSecond		: int8


	for e in face do
		if ( e.isElem1Halo ) then
			-- Elem0 and Elem1 are not in the same graph partition
			-- Region reduction is needed for Elem1's surfRes
			-- Now we may have two cases where
			-- 1) Both elements are same time level or
			-- 2) Elem1 has higher time level
			-- For both cases, we need to calculate surface 
			-- residual contribution of current face based on
			-- ltsLevel and factTimeLev value.
			-- For example, for three time level case
			--                   ltsLevel
			--             0      1      2      3
			-- ------------------------------------
			-- timeLev0    O      O      O      O
			-- timeLev1    X      O      X      O
			-- timeLev2    X      X      X      O
			elem0Cont = ((ltsLevel+1)/q[e.elem0].factTimeLevRev)*q[e.elem0].factTimeLevRev-(ltsLevel+1)
			if ( elem0Cont == 0 ) then
				elem1Cont = ((ltsLevel+1)/q[e.elem1].factTimeLevRev)*q[e.elem1].factTimeLevRev-(ltsLevel+1)

				-- 1) Both elements are same time level
				if not ( e.isDiffTimeLev ) then
					cellNum = e.elem0

					-- Determine face orientation
					lB0 = (e.face0-1)*Nfp
					uB0 = e.face0*Nfp
					isFlipped = false
					indValGrad = 1
					if ( e.face1 > 9 ) then
						-- Elem1 side is flipped
						lB1 = (e.face1/10-1)*Nfp
						uB1 = e.face1*Nfp/10
						isFlipped = true
					else
						lB1 = (e.face1-1)*Nfp
						uB1 = e.face1*Nfp
					end

					for ii=0,nTimeInt do
						-- Temporal weight for surface residual calculation
						halfWeight = 0.5*wTimeInt[ii].v

						-- Interpolate predictor solution of elem0
						for kk=0,Np do
							solIntRho0[kk]	= 0.0
							solIntRhou0[kk]	= 0.0
							solIntRhov0[kk]	= 0.0
							solIntEner0[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rho
								solIntRhou0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhou
								solIntRhov0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhov
								solIntEner0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QM values
						indVal = 0
						for kk=lB0,uB0 do
							QMRho[indVal]	= solIntRho0[vmapM[kk].v]
							QMRhou[indVal]	= solIntRhou0[vmapM[kk].v]
							QMRhov[indVal]	= solIntRhov0[vmapM[kk].v]
							QMEner[indVal]	= solIntEner0[vmapM[kk].v]
							indVal = indVal + 1
						end
        
						-- Interpolate predictor solution of elem1
						for kk=0,Np do
							solIntRho1[kk]	= 0.0
							solIntRhou1[kk]	= 0.0
							solIntRhov1[kk]	= 0.0
							solIntEner1[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].rho
								solIntRhou1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].rhou
								solIntRhov1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].rhov
								solIntEner1[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem1].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QP values 
						indVal = 0
						indValGrad = 1
						if ( isFlipped ) then
							indVal = Nfp-1
							indValGrad = -1						
						end
    
						for kk=lB1,uB1 do
							QPRho[indVal]	= solIntRho1[vmapM[kk].v]
							QPRhou[indVal]	= solIntRhou1[vmapM[kk].v]
							QPRhov[indVal]	= solIntRhov1[vmapM[kk].v]
							QPEner[indVal]	= solIntEner1[vmapM[kk].v]
							indVal = indVal + indValGrad
						end
        
						-- Calculate surface fluxes
						Euler2DLF(cellNum, p_space, lB0, F1s, F2s, F3s, F4s, q[cellNum].nx, q[cellNum].ny, QMRho, QMRhou, QMRhov, QMEner, QPRho, QPRhou, QPRhov, QPEner)

						-- Update elem0 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							for ll=lB0,uB0 do
								tempValRho	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F1s[indVal]
								tempValRhou	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F2s[indVal]
								tempValRhov	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F3s[indVal]
								tempValEner	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F4s[indVal]
								indVal = indVal + 1 
							end
            
							q[e.elem0].surfRes[kk].rho	+= (halfWeight*tempValRho)
							q[e.elem0].surfRes[kk].rhou	+= (halfWeight*tempValRhou)
							q[e.elem0].surfRes[kk].rhov	+= (halfWeight*tempValRhov)
							q[e.elem0].surfRes[kk].ener	+= (halfWeight*tempValEner)
						end

						-- Update elem1 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							indValGrad = 1
							if ( isFlipped ) then
								indVal = Nfp-1
								indValGrad = -1						
							end
							for ll=lB1,uB1 do
								-- Note that signs are flipped compared to elem0
								tempValRho	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F1s[indVal]
								tempValRhou	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F2s[indVal]
								tempValRhov	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F3s[indVal]
								tempValEner	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F4s[indVal]
								indVal = indVal + indValGrad
							end
							q[e.elem1].surfRes[kk].rho	+= (halfWeight*tempValRho)
							q[e.elem1].surfRes[kk].rhou	+= (halfWeight*tempValRhou)
							q[e.elem1].surfRes[kk].rhov	+= (halfWeight*tempValRhov)
							q[e.elem1].surfRes[kk].ener	+= (halfWeight*tempValEner)
						end
					end
				-- 2) Elem1 has higher time level
				else
					cellNum = e.elem0
					if ( elem1Cont == 0 ) then
						isSecond = 1
					else
						isSecond = 0
					end

					-- Determine face orientation
					lB0 = (e.face0-1)*Nfp
					uB0 = e.face0*Nfp
					isFlipped = false
					indValGrad = 1
					if ( e.face1 > 9 ) then
						-- Elem1 side is flipped
						lB1 = (e.face1/10-1)*Nfp
						uB1 = e.face1*Nfp/10
						isFlipped = true
					else
						lB1 = (e.face1-1)*Nfp
						uB1 = e.face1*Nfp
					end

					for ii=0,nTimeInt do
						-- Temporal weight for surface residual calculation
						halfWeight = 0.5*wTimeInt[ii].v
						quartWeight = 0.25*wTimeInt[ii].v

						-- Interpolate predictor solution of elem0
						for kk=0,Np do
							solIntRho0[kk]	= 0.0
							solIntRhou0[kk]	= 0.0
							solIntRhov0[kk]	= 0.0
							solIntEner0[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rho
								solIntRhou0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhou
								solIntRhov0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].rhov
								solIntEner0[ll]	+= DOFToIntTime[ii*Nt+kk].v*q[e.elem0].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QM values
						indVal = 0
						for kk=lB0,uB0 do
							QMRho[indVal]	= solIntRho0[vmapM[kk].v]
							QMRhou[indVal]	= solIntRhou0[vmapM[kk].v]
							QMRhov[indVal]	= solIntRhov0[vmapM[kk].v]
							QMEner[indVal]	= solIntEner0[vmapM[kk].v]
							indVal = indVal + 1
						end
        
						-- Interpolate predictor solution of elem1
						for kk=0,Np do
							solIntRho1[kk]	= 0.0
							solIntRhou1[kk]	= 0.0
							solIntRhov1[kk]	= 0.0
							solIntEner1[kk]	= 0.0
						end
						indVal = 0
						for kk=0,Nt do
							for ll=0,Np do
								solIntRho1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].rho
								solIntRhou1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].rhou
								solIntRhov1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].rhov
								solIntEner1[ll]	+= DOFToIntAdjTime[ii*Nt+isSecond*nTimeIntNt+kk].v*q[e.elem1].preSol[indVal].ener
								indVal = indVal + 1
							end
						end

						-- Assign QP values 
						indVal = 0
						indValGrad = 1
						if ( isFlipped ) then
							indVal = Nfp-1
							indValGrad = -1						
						end
    
						for kk=lB1,uB1 do
							QPRho[indVal]	= solIntRho1[vmapM[kk].v]
							QPRhou[indVal]	= solIntRhou1[vmapM[kk].v]
							QPRhov[indVal]	= solIntRhov1[vmapM[kk].v]
							QPEner[indVal]	= solIntEner1[vmapM[kk].v]
							indVal = indVal + indValGrad
						end
        
						-- Calculate surface fluxes
						Euler2DLF(cellNum, p_space, lB0, F1s, F2s, F3s, F4s, q[cellNum].nx, q[cellNum].ny, QMRho, QMRhou, QMRhov, QMEner, QPRho, QPRhou, QPRhov, QPEner)

						-- Update elem0 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							for ll=lB0,uB0 do
								tempValRho	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F1s[indVal]
								tempValRhou	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F2s[indVal]
								tempValRhov	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F3s[indVal]
								tempValEner	-= LIFT[kk*NfpSum+ll].v*q[e.elem0].Fscale[ll]*F4s[indVal]
								indVal = indVal + 1 
							end
            
							q[e.elem0].surfRes[kk].rho	+= (halfWeight*tempValRho)
							q[e.elem0].surfRes[kk].rhou	+= (halfWeight*tempValRhou)
							q[e.elem0].surfRes[kk].rhov	+= (halfWeight*tempValRhov)
							q[e.elem0].surfRes[kk].ener	+= (halfWeight*tempValEner)
						end

						-- Update elem1 surface residual
						for kk=0,Np do
							tempValRho	= 0.0
							tempValRhou	= 0.0
							tempValRhov	= 0.0
							tempValEner	= 0.0
							indVal = 0
							indValGrad = 1
							if ( isFlipped ) then
								indVal = Nfp-1
								indValGrad = -1						
							end
							for ll=lB1,uB1 do
								-- Note that signs are flipped compared to elem0
								tempValRho	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F1s[indVal]
								tempValRhou	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F2s[indVal]
								tempValRhov	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F3s[indVal]
								tempValEner	+= LIFT[kk*NfpSum+ll].v*q[e.elem1].Fscale[ll]*F4s[indVal]
								indVal = indVal + indValGrad
							end
							q[e.elem1].surfRes[kk].rho	+= (quartWeight*tempValRho)
							q[e.elem1].surfRes[kk].rhou	+= (quartWeight*tempValRhou)
							q[e.elem1].surfRes[kk].rhov	+= (quartWeight*tempValRhov)
							q[e.elem1].surfRes[kk].ener	+= (quartWeight*tempValEner)
						end
					end
				end
			end
		end
	end

	-- Free buffers
	c.free(solIntRho0)
	c.free(solIntRhou0)
	c.free(solIntRhov0)
	c.free(solIntEner0)
	c.free(solIntRho1)
	c.free(solIntRhou1)
	c.free(solIntRhov1)
	c.free(solIntEner1)
	c.free(QMRho)
	c.free(QMRhou)
	c.free(QMRhov)
	c.free(QMEner)
	c.free(QPRho)
	c.free(QPRhou)
	c.free(QPRhov)
	c.free(QPEner)
	c.free(F1s)
	c.free(F2s)
	c.free(F3s)
	c.free(F4s)
end

task residualVolume(ltsLevel : uint8, p_space : int8, Np : uint64, Nt : uint64, nTimeInt : uint64, wTimeInt : region(ispace(int1d),doubleVal), DOFToIntTime : region(ispace(int1d),doubleVal), Drw : region(ispace(int1d),doubleVal), Dsw : region(ispace(int1d),doubleVal), q : region(ispace(ptr),Elem))
where
	reads(wTimeInt.v, DOFToIntTime.v, Drw.v, Dsw.v, q.cellInd, q.factTimeLevRev, q.volRes, q.preSol, q.rx, q.sx, q.ry, q.sy),
	writes(q.volRes)
do
 	var solIntRho		:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np, Ps=9
 	var solIntRhou		:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var solIntRhov		:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var solIntEner		:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var F1				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var F2				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var F3				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var F4				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var G1				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var G2				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var G3				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var G4				:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var dFdr			:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var dFds			:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var dGdr			:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
 	var dGds			:&double = [&double](c.malloc(Np*[terralib.sizeof(double)]))		-- Np
	var Nfp				: uint64 = p_space+1
	var NfpSum			: uint64 = 3*Nfp 
	var gamma			: double = 1.4
	var halfWeight		: double
	var tempVal			: double
	var cellNum			: uint64
	var elem0Cont		: int8

	for e in q do
		cellNum = e.cellInd
		elem0Cont = ((ltsLevel+1)/q[cellNum].factTimeLevRev)*q[cellNum].factTimeLevRev-(ltsLevel+1)

		if ( elem0Cont == 0 ) then
			-- Looping over temporal integration points
			for ii=0,nTimeInt do
				-- Temporal weight for volume residual calculation
				halfWeight = 0.5*wTimeInt[ii].v
    
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
					q[cellNum].volRes[kk].rho += halfWeight*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
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
					q[cellNum].volRes[kk].rhou += halfWeight*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
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
					q[cellNum].volRes[kk].rhov += halfWeight*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
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
					q[cellNum].volRes[kk].ener += halfWeight*( q[cellNum].rx[kk]*dFdr[kk] + q[cellNum].sx[kk]*dFds[kk] + q[cellNum].ry[kk]*dGdr[kk] + q[cellNum].sy[kk]*dGds[kk] )
				end
			end
		end
	end

	-- Free buffers
	c.free(solIntRho)
	c.free(solIntRhou)
	c.free(solIntRhov)
	c.free(solIntEner)
	c.free(F1)
	c.free(F2)
	c.free(F3)
	c.free(F4)
	c.free(G1)
	c.free(G2)
	c.free(G3)
	c.free(G4)
	c.free(dFdr)
	c.free(dFds)
	c.free(dGdr)
	c.free(dGds)
end

task Euler2DUpdateSolution(ltsLevel : uint8, dt : double, Np : uint64, q : region(ispace(ptr),Elem))
where
	reads(q.sol,q.volRes,q.surfRes,q.cellInd,q.factTimeLev,q.factTimeLevRev),
	writes(q.sol,q.volRes,q.surfRes)
do
	var cellNum		: uint64
	var elem0Cont	: int8
	var dtLoc		: double
	for e in q do
		cellNum = e.cellInd
		elem0Cont = ((ltsLevel+1)/q[cellNum].factTimeLevRev)*q[cellNum].factTimeLevRev-(ltsLevel+1)
		if ( elem0Cont == 0 ) then
			dtLoc = dt/q[cellNum].factTimeLev
			for kk=0,Np do
				q[cellNum].sol[kk].rho  += dtLoc*(q[cellNum].volRes[kk].rho + q[cellNum].surfRes[kk].rho)
				q[cellNum].sol[kk].rhou += dtLoc*(q[cellNum].volRes[kk].rhou + q[cellNum].surfRes[kk].rhou)
				q[cellNum].sol[kk].rhov += dtLoc*(q[cellNum].volRes[kk].rhov + q[cellNum].surfRes[kk].rhov)
				q[cellNum].sol[kk].ener += dtLoc*(q[cellNum].volRes[kk].ener + q[cellNum].surfRes[kk].ener)

				-- Set volume and surface residuals to zero
                q[cellNum].volRes[kk].rho = 0.0
                q[cellNum].volRes[kk].rhou = 0.0
                q[cellNum].volRes[kk].rhov = 0.0
                q[cellNum].volRes[kk].ener = 0.0
                q[cellNum].surfRes[kk].rho = 0.0
                q[cellNum].surfRes[kk].rhou = 0.0
                q[cellNum].surfRes[kk].rhov = 0.0
                q[cellNum].surfRes[kk].ener = 0.0
			end
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
	var NFaces				: uint64
    var Nt					: uint64
	var nSpaceInt			: uint64
	var nTimeInt			: uint64
	var nTimeLev			: uint64
	var nLTSStep			: uint64
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
	var stdElemSpaceFileNameLocal	: int8[64]
	var stdElemTimeFileNameLocal	: int8[64]
	var aderIterMatFileNameLocal	: int8[64]
	var meshFileNameLocal			: int8[64]
	var EToEFileNameLocal			: int8[64]
	var EToFFileNameLocal			: int8[64]
	var faceFileNameLocal			: int8[64]
	var partFileNameLocal			: int8[64]


	-- 2) Get *.config input and assign values
	var config : su2Config
	config:initializeFromCommand()
	finalTime	= config.finalTime
	CFL			= config.CFL
	nTimeLev	= config.nTimeLev
	nLTSStep	= pow(2,nTimeLev-1)
	nDOFs		= NpG
	Nfp			= NfpG
	NFaces		= NFacesG
	Nt			= NtG
	nSpaceInt	= nSpaceIntG
	nTimeInt	= nTimeIntG
	gridK		= gridKG
	gridNv		= gridNvG
	cstring.strcpy(stdElemSpaceFileNameLocal,stdElemSpaceFileName)
	cstring.strcpy(stdElemTimeFileNameLocal,stdElemTimeFileName)
	cstring.strcpy(aderIterMatFileNameLocal,aderIterMatFileName)
	cstring.strcpy(meshFileNameLocal,meshFileName)
	cstring.strcpy(EToEFileNameLocal,EToEFileName)
	cstring.strcpy(EToFFileNameLocal,EToFFileName)
	cstring.strcpy(faceFileNameLocal,faceFileName)
	cstring.strcpy(partFileNameLocal,partFileName)
	cstring.strcat(partFileNameLocal,config.partFileTail)
	var colors		= ispace(int1d, config.parallelism)
	var colorsTime	= ispace(int1d, nTimeLev)


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
	var DOFToIntAdjTime= region(ispace(int1d,2*nTimeInt*Nt), doubleVal)
	var AderIterMat	= region(ispace(int1d,(nDOFs*Nt)*(nDOFs*Nt)), doubleVal)
	var vmapM		= region(ispace(int1d,3*Nfp), uintVal)
	readStdElemSpaceInfo(stdElemSpaceFileNameLocal, MSpace, Dr, Ds, Drw, Dsw, LIFT, DrSpaceInt, DsSpaceInt, wSpaceInt, DOFToIntSpaceTranspose, vmapM)
	readStdElemTimeInfo(stdElemTimeFileNameLocal, lFirst, wTimeInt, DOFToIntTime, DOFToIntAdjTime)
	readAderIterMatInfo(aderIterMatFileNameLocal, AderIterMat)


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
	var coloring10 = c.legion_domain_point_coloring_create()

	for ii=0,config.parallelism do
		c.legion_domain_point_coloring_color_domain(coloring1, [int1d](ii), rect1d {0,nDOFs*nDOFs-1})		-- MSpace, Dr, Ds, Drw, Dsw
		c.legion_domain_point_coloring_color_domain(coloring2, [int1d](ii), rect1d {0,3*Nfp*nDOFs-1})		-- LIFT
		c.legion_domain_point_coloring_color_domain(coloring3, [int1d](ii), rect1d {0,nSpaceInt*nDOFs-1})	-- DrSpaceInt, DsSpaceInt, DOFToIntSpaceTranspose
		c.legion_domain_point_coloring_color_domain(coloring4, [int1d](ii), rect1d {0,nSpaceInt-1})			-- wSpaceInt
		c.legion_domain_point_coloring_color_domain(coloring5, [int1d](ii), rect1d {0,Nt-1})				-- lFirst
		c.legion_domain_point_coloring_color_domain(coloring6, [int1d](ii), rect1d {0,nTimeInt-1})			-- wTimeInt
		c.legion_domain_point_coloring_color_domain(coloring7, [int1d](ii), rect1d {0,nTimeInt*Nt-1})		-- DOFTonIntTime
		c.legion_domain_point_coloring_color_domain(coloring8, [int1d](ii), rect1d {0,2*nTimeInt*Nt-1})		-- DOFTonIntAdjTime
		c.legion_domain_point_coloring_color_domain(coloring9, [int1d](ii), rect1d {0,nDOFs*Nt*nDOFs*Nt-1})	-- AderIterMat
		c.legion_domain_point_coloring_color_domain(coloring10, [int1d](ii), rect1d {0,3*Nfp-1})				-- vmapM
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
	var DOFToIntAdjTimePart			= partition(aliased,DOFToIntAdjTime,coloring8,ispace(int1d,config.parallelism))
	var AderIterMatPart				= partition(aliased,AderIterMat,coloring9,ispace(int1d,config.parallelism))
	var vmapMPart					= partition(aliased,vmapM,coloring10,ispace(int1d,config.parallelism))
	c.legion_domain_point_coloring_destroy(coloring1)
	c.legion_domain_point_coloring_destroy(coloring2)
	c.legion_domain_point_coloring_destroy(coloring3)
	c.legion_domain_point_coloring_destroy(coloring4)
	c.legion_domain_point_coloring_destroy(coloring5)
	c.legion_domain_point_coloring_destroy(coloring6)
	c.legion_domain_point_coloring_destroy(coloring7)
	c.legion_domain_point_coloring_destroy(coloring8)
	c.legion_domain_point_coloring_destroy(coloring9)
	c.legion_domain_point_coloring_destroy(coloring10)


	-- 4) Read mesh
	var	gridVertex		= region(ispace(ptr,gridNv), GridVertex)
	var gridEToV		= region(ispace(ptr,gridK), GridEToV(wild))
	var gridVertexEqual	= partition(equal,gridVertex,colors)
	var gridEToVEqual	= partition(equal,gridEToV,colors)

    -- Read vertices info
	c.printf("--------------Read Vertices Info------------\n\n")
	for color in colors do
		readVertex(meshFileNameLocal,gridVertexEqual[color])
	end

	-- Read EToV info
	c.printf("----------Read Elem To Vertex Info----------\n\n")
	for color in colors do
		readElemToVertex(meshFileNameLocal,gridNv,gridVertex,gridEToVEqual[color])
	end


	-- 5) Declare regions
	var q			= region(ispace(ptr,gridK), Elem)
	var face		= region(ispace(ptr,NFaces), surf)
	var qEqual		= partition(equal,q,colors)
	var faceEqual	= partition(equal,face,colors)


	-- 6) Read connectivity info and graph partition info
	c.printf("---------Generate Connectiviy Info----------\n\n")
	for color in colors do
		generateQPConnectivity(EToEFileNameLocal, EToFFileNameLocal, Nfp, qEqual[color])
	end
	c.printf("---------------Color Elements---------------\n\n")
	for color in colors do
		colorElem(partFileNameLocal, config.parallelism, gridVertex, gridEToVEqual[color],qEqual[color])
	end
	c.printf("--------------Read Faces Info---------------\n\n")
	for color in colors do
		readFaceInfo(faceFileNameLocal,faceEqual[color])
	end


	-- 7) Final partitioning based on cellColor and faceColor value
	var qPart		= partition(q.cellColor, colors)
	var qParte1		= preimage(q,qPart,q.e1)
	var qParte2		= preimage(q,qPart,q.e2)
	var qParte3		= preimage(q,qPart,q.e3)
	var qPartHalo	= ( qParte1 | qParte2 | qParte3 )

	var gridEToVPart	= partition(gridEToV.cellColor, colors)
	var gridVertexPart1	= image(gridVertex, gridEToVPart, gridEToV.v1)
	var gridVertexPart2	= image(gridVertex, gridEToVPart, gridEToV.v2)
	var gridVertexPart3	= image(gridVertex, gridEToVPart, gridEToV.v3)
	var gridVertexPart	= gridVertexPart1 | gridVertexPart2 | gridVertexPart3


	-- 8) Preprocessing and initialize the solution
	c.printf("----------Build Internal DOFs Info----------\n\n")
	for color in colors do
		buildNodes(p_space,nTimeLev,gridVertexPart[color],gridEToVPart[color],qPart[color])
	end
	c.printf("---Calculate Jacobians And Normal Vectors---\n\n")
	for color in colors do
		calcGeoFacAndNormal(p_space,nSpaceInt,Dr,Ds,DrSpaceInt,DsSpaceInt,qEqual[color])
	end
	c.printf("-----------------Color Faces----------------\n\n")
	for color in colors do
		colorFaces(q, faceEqual[color])
	end
	c.printf("------------Initialize Solution-------------\n\n")
	for color in colors do
		solutionAtTimeT(0.0,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,config.p0Val,qEqual[color])
	end
	var facePart		= partition(face.faceColorGraph, colors)
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

		-- Calculate predictor solutions for all time levels
		for color in colors do
			Euler2DPredictorWrapper(0,nDOFs,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpacePart[color],DrSpaceIntPart[color],DsSpaceIntPart[color],wSpaceIntPart[color],DOFToIntSpaceTransposePart[color],lFirstPart[color],wTimeIntPart[color],DOFToIntTimePart[color],AderIterMatPart[color],qPart[color])
		end

		-- Calculate surface residual contribution from owned elements
		for color in colors do
			residualSurfaceOwned(0,p_space,nDOFs,Nt,nTimeInt,wTimeIntPart[color],DOFToIntTimePart[color],DOFToIntAdjTimePart[color],LIFTPart[color],vmapMPart[color],qPart[color],facePart[color])
		end

		-- Calculate surface residual contribution from halo elements
		for color in colors do
			residualSurfaceHalo(0,p_space,nDOFs,Nt,nTimeInt,wTimeIntPart[color],DOFToIntTimePart[color],DOFToIntAdjTimePart[color],LIFTPart[color],vmapMPart[color],qPartHalo[color],facePart[color])		
		end

		-- Calculate volume residual contribution for timeLev=0
		for color in colors do
			residualVolume(0,p_space,nDOFs,Nt,nTimeInt,wTimeIntPart[color],DOFToIntTimePart[color],DrwPart[color],DswPart[color],qPart[color])
		end

		-- Update the solution for timeLev == 0 
		for color in colors do
			Euler2DUpdateSolution(0,dt,nDOFs,qPart[color])
		end

		-- If there is only one time level, following loop will be omitted
		for ii=1, nLTSStep do
			-- Calculate predictor soutions
			for color in colors do
				Euler2DPredictorWrapper(ii,nDOFs,Nt,nSpaceInt,nTimeInt,dt,tolSolAderRho,tolSolAderRhouRhov,tolSolAderEner,MSpacePart[color],DrSpaceIntPart[color],DsSpaceIntPart[color],wSpaceIntPart[color],DOFToIntSpaceTransposePart[color],lFirstPart[color],wTimeIntPart[color],DOFToIntTimePart[color],AderIterMatPart[color],qPart[color])
			end

			-- Calculate surface residual contribution from owned elements
			for color in colors do
				residualSurfaceOwned(ii,p_space,nDOFs,Nt,nTimeInt,wTimeIntPart[color],DOFToIntTimePart[color],DOFToIntAdjTimePart[color],LIFTPart[color],vmapMPart[color],qPart[color],facePart[color])
			end

			-- Calculate surface residual contribution from halo elements
			for color in colors do
				residualSurfaceHalo(ii,p_space,nDOFs,Nt,nTimeInt,wTimeIntPart[color],DOFToIntTimePart[color],DOFToIntAdjTimePart[color],LIFTPart[color],vmapMPart[color],qPartHalo[color],facePart[color])		
			end

			-- Calculate volume residual contribution
			for color in colors do
				residualVolume(ii,p_space,nDOFs,Nt,nTimeInt,wTimeIntPart[color],DOFToIntTimePart[color],DrwPart[color],DswPart[color],qPart[color])
			end

			-- Update the solution for timeLev == 0 
			for color in colors do
				Euler2DUpdateSolution(ii,dt,nDOFs,qPart[color])
			end
		end

		-- Marching one time step
--		c.printf("simTime = %20.12lf\n",simTime)	-- This line should be commented if SPMD is used
		simTime += dt
	end
	__fence(__execution, __block)
	var ts_stop = c.legion_get_current_time_in_micros()
	c.printf("simTime = %9.4lf\n",simTime)
	c.printf("\nEuler2D simultation completed in %.4f sec\n\n",(ts_stop - ts_start) * 1e-6)


	--10) Calculate error
    for color in colors do
        L1Error     +=  calcL1Error(simTime,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,qEqual[color])
    end
    for color in colors do
        L2Error     +=  calcL2Error(simTime,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,qEqual[color])
    end
    for color in colors do
        LInfError   max= calcLInfError(simTime,nDOFs,config.epsVal,config.rho0Val,config.u0Val,config.v0Val,qEqual[color])
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

