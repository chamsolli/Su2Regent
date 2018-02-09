import "regent"
local c = regentlib.c
local cstring = terralib.includec("string.h")

struct su2Config
{
	configFileName			: int8[64],
	meshFileFormat			: int8[64],
	meshFileName			: int8[64],
	simCase					: int8[64],
	p_space					: uint64,
	p_time					: uint64,
	fluxTypeDG				: int8[16],
	timeStepping			: int8[16],
	CFL 					: double,
	gamma					: double,
	finalTime				: double,
	xPeriodicCenter			: double,
	xPeriodicTranslation	: double,
	yPeriodicCenter			: double,
	yPeriodicTranslation	: double,
	parallelism				: uint64,
	partFileTail			: int8[64],
	nDOFs					: uint64,
	Nfp						: uint64,
	Nt						: uint64,
	epsVal					: double,
	rho0Val					: double,
	u0Val					: double,
	v0Val					: double,
	p0Val					: double
}

terra printUsageAndAbort()
	c.printf("Usage: regent.py SU2_CFD.rg [OPTIONS]\n")
	c.printf("OPTIONS\n")
	c.printf("  -h            : Print the usage and exit.\n")
	c.printf("  -p {value}    : Set the number of parallel tasks to {value}.\n")
	c.abort()
end

terra readConfigFile(configFileNameInput : &int8, self : &su2Config)
	var strIn1			: int8[512]
	var strIn2			: int8[512]
	var strIn3			: int8[512]
	var strHis			: int8[512]
	var isOptionScanned	: bool = true

	cstring.strcpy(strHis,"")
	var f = c.fopen(configFileNameInput,"r")
	
    c.printf("--------------Read Config File--------------\n")
	for ii=0,1000 do
		c.fscanf(f,"%s%s%s",&strIn1,&strIn2,&strIn3)
		isOptionScanned = false
		if cstring.strcmp(strIn1,strHis) == 0 then  -- End of File
			c.fclose(f)
			break
		else
			cstring.strcpy(strHis,strIn1)
		end
		if ( strIn3[0] == 61 ) then
			c.fclose(f)
			c.printf("Error in '%s' file. Check config option '%s'\n",strIn1)
			c.abort()
		end

		-- Assign user defined options
		if cstring.strcmp(strIn1,"meshFileFormat") == 0 then
			cstring.strcpy(self.meshFileFormat,strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"meshFileName") == 0 then
			cstring.strcpy(self.meshFileName,strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"simCase") == 0 then
			cstring.strcpy(self.simCase,strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"fluxTypeDG") == 0 then
			cstring.strcpy(self.fluxTypeDG,strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"timeStepping") == 0 then
			cstring.strcpy(self.timeStepping,strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"p_space") == 0 then
			self.p_space = [uint64](c.atoi(strIn3))
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"p_time") == 0 then
			self.p_time = [uint64](c.atoi(strIn3))
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"CFL") == 0 then
			self.CFL = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"gamma") == 0 then
			self.gamma = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"finalTime") == 0 then
			self.finalTime = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"xPeriodicCenter") == 0 then
			self.xPeriodicCenter = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"xPeriodicTranslation") == 0 then
			self.xPeriodicTranslation = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"yPeriodicCenter") == 0 then
			self.yPeriodicCenter = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"yPeriodicTranslation") == 0 then
			self.yPeriodicTranslation = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"epsVal") == 0 then
			self.epsVal = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"rho0Val") == 0 then
			self.rho0Val = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"u0Val") == 0 then
			self.u0Val = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"v0Val") == 0 then
			self.v0Val = c.atof(strIn3)
			isOptionScanned = true
		end
		if cstring.strcmp(strIn1,"p0Val") == 0 then
			self.p0Val = c.atof(strIn3)
			isOptionScanned = true
		end
		if not(isOptionScanned) then
			c.printf("Current option '%s' doesn't exist\n")
			c.abort()
		end
	end

	self.nDOFs	= (self.p_space+1)*(self.p_space+2)/2
	self.Nfp	= self.p_space + 1

	c.printf("Config file = %s\n",configFileNameInput)
	c.printf("Mesh file = %s\n",self.meshFileName)
	c.printf("p_space = %llu\n",self.p_space)
	c.printf("p_time = %llu\n",self.p_time)
	c.printf("CFL = %lf\n",self.CFL)
	c.printf("fluxTypeDG = %s\n",self.fluxTypeDG)
	c.printf("timeStepping = %s\n",self.timeStepping)
	c.printf("DOF in each triangle = %llu\n",self.nDOFs)
	c.printf("Nfp in each face = %llu\n",self.Nfp)
end

terra su2Config:initializeFromCommand()
	var tsStart = c.legion_get_current_time_in_micros()

	var configInputGiven = false
	cstring.strcpy(self.configFileName,"default.cfg")
	self.parallelism	= 1
	self.epsVal			= 5.0
	self.rho0Val		= 1.0
	self.u0Val			= 1.0
	self.v0Val			= 1.0
	self.p0Val			= 1.0

	var args = c.legion_runtime_get_input_args()
	var i = 1
	while i < args.argc do
		if cstring.strcmp(args.argv[i], "-h") == 0 then
			printUsageAndAbort()
		elseif cstring.strcmp(args.argv[i], "-p") == 0 then
			i = i + 1
			self.parallelism = [uint64](c.atoi(args.argv[i]))
			if not (self.parallelism == 1) then
				if ( self.parallelism == 2 ) then
					cstring.strcpy(self.partFileTail,"2.dat")
				elseif ( self.parallelism == 4 ) then
					cstring.strcpy(self.partFileTail,"4.dat")
				elseif ( self.parallelism == 8 ) then
					cstring.strcpy(self.partFileTail,"8.dat")
				elseif ( self.parallelism == 16 ) then
					cstring.strcpy(self.partFileTail,"16.dat")
				elseif ( self.parallelism == 32 ) then
					cstring.strcpy(self.partFileTail,"32.dat")
				elseif ( self.parallelism == 64 ) then
					cstring.strcpy(self.partFileTail,"64.dat")
				elseif ( self.parallelism == 128 ) then
					cstring.strcpy(self.partFileTail,"128.dat")
				elseif ( self.parallelism == 256 ) then
					cstring.strcpy(self.partFileTail,"256.dat")
				else
					c.printf("Doesn't support given parallelism\n")
					c.printf("Change parallelism values to 2, 4, 8, 16, 32, 64, 128 or 256\n")
					c.abort()
				end
			else
				cstring.strcpy(self.partFileTail,"")
			end
		end
		i = i + 1
	end

	-- Try to read 'default.cfg' file
	var file = c.fopen("default.cfg","rb")
	if file == nil then
		c.printf("Configuration file 'default.cfg' doesn't exist!\n")
		c.fclose(file)
		printUsageAndAbort()
	end
	c.fclose(file)
	configInputGiven = true
	cstring.strcpy(self.configFileName,"default.cfg")
	readConfigFile("default.cfg",self)

	if not configInputGiven then
		c.printf("Configuration file 'default.cfg' is missing!!!\n")
		printUsageAndAbort()
	end
	var tsStop = c.legion_get_current_time_in_micros()
	c.printf("\nRead configuration took %.4f sec\n\n", (tsStop-tsStart) * 1e-6)
end

return su2Config
