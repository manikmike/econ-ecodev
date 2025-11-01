module FortranEViewsModEx
   
   use EViewsMgr
   implicit none
   
   public :: FortranEViewsInitialize, FortranEViewsUninitialize ! Must always be called in client code
   public :: OpenEViewsWorkfile, CloseEViewsWorkfile, FetchEViewsSeries, FetchEViewsSeriesVersion
   public :: StoreEViewsSeries, StoreEViewsSeriesVersion, GetEViewsString
   public :: CreateEViewsWorkfile
   public :: ImportModelSolution
   public :: ExecuteEViewsProgram
   private :: FortranEViewsTest, seriesNames ! Made private after testing
   public :: per1, per2
   public :: MAX_YR, MAX_QTR, MAX_MTH
   public :: MAX_LIST_SIZE, LIST_ITEM_LEN

   private
   
   ! Series dimensions
   integer, parameter :: MAX_YEAR = 2110
   integer, parameter :: MAX_YR = 210     ! (MAX_YEAR - 1900)
   integer, parameter :: MAX_QTR = 843    !  4 * MAX_YR + 3
   integer, parameter :: MAX_MTH = 2531   ! 12 * MAX_YR + 11

   ! Legacy parameters
   integer, parameter :: MAX_LIST_SIZE = 2000
   integer, parameter :: LIST_ITEM_LEN = 256
   
   integer :: status
   integer(int_ptr_kind()) :: mgr
   integer(int_ptr_kind()) :: app
   logical :: initialized = .false.
   character (len=500000) :: output
   character (len=32), dimension(40000) :: seriesNames
   integer :: per1, per2
   
   ! These methods are described in the EViews COM Automation Whitepaper - March 10, 2015
   ! Some of their argument lists have been modified to match the Fortran Module Wizard output
   private :: Show, Hide, ShowLog, HideLog, Run, Lookup
   private :: ListToArray, ArrayToList
   private :: Get, Get2D, GetSeries, GetSeries2D, GetGroup, GetGroupEx
   private :: Put, PutSeries, PutGroup
   
   ! Enumeration types from EViewsMgr.f90 (module EViewsMgr)
   private :: NewInstance, ExistingOrNew, ExistingOnly
   private :: DataTypeAuto, DataTypeScalar, DataTypeScalarAlpha, &
              DataTypeSeries, DataTypeSeriesAlpha, DataTypeVector, &
              DataTypeVectorCoefficient, DataTypeVectorAlpha, DataTypeMatrix, &
              DataTypeMatrixSymmetric
   private :: EViewsEventTypeNone, EViewsEventTypeAll, EViewsEventTypeStatus, &
              EViewsEventTypeError, EViewsEventTypeProgramLogAll, &
              EViewsEventTypeProgramLogMsgs, EViewsEventTypeProgramLogWarnings, &
              EViewsEventTypeProgramLogLines, EViewsEventTypeProgramLogProfile, &
              EViewsEventTypeXLog
   private :: LookupReturnString, LookupReturnArray, &
              LookupReturnMatrixAsRows, LookupReturnMatrixAsColumns
   private :: NATypeAsEmpty, NATypeAsString, NATypeAsExcelNA
   private :: SeriesTypeAuto, SeriesTypeSeries, SeriesTypeAlpha
   private :: WriteUpdate, WriteOverwrite, WriteMergePreferSource, &
              WriteMergePreferDestination, WriteProtect
   
   ! SafeArray functions from oleaut32.f90 (module oleaut32 via ifcom via EViewMgr)
   ! https://msdn.microsoft.com/en-us/library/windows/desktop/ms221145(v=vs.85).aspx
   private :: SafeArrayGetElement, SafeArrayGetLBound, SafeArrayGetUBound, &
              SafeArrayGetElemSize, SafeArrayCreate, SafeArrayPutElement
   
   ! SafeArray bounds and Variant type/constants from ifwinty.f90 (module ifwinty via EViewsMgr)
   ! https://msdn.microsoft.com/en-us/library/windows/desktop/ms221167(v=vs.85).aspx
   ! https://msdn.microsoft.com/en-us/library/windows/desktop/ms221627(v=vs.85).aspx
   private :: sa_bounds, variant, VT_ARRAY, VT_VARIANT, VT_R8
   
   ! Convenience routines for converting to/from BSTRs from ifcom.f90 (module ifcom via EViewsMgr)
   ! https://msdn.microsoft.com/en-us/library/t58y75c0(v=vs.100).aspx
   ! https://msdn.microsoft.com/en-us/library/ewezf1f6(v=vs.100).aspx
   private :: ConvertStringToBSTR, ConvertBSTRtoString
   
contains

!==============================================================================

   subroutine FortranEViewsInitialize()
   
      call Initialize()
      !call Show()
      !call ShowLog()
      call Run("mode quiet")
      call Run("wfcreate(wf=c:\fevdata\fevdata, page=Annual) a 1901 2110")
      call Run("pagecreate(page=Quarterly) q 1900Q2 2110Q4")
      call Run("pagecreate(page=Monthly) m 1900M2 2110M12")
   
   end subroutine FortranEViewsInitialize

!==============================================================================

   subroutine FortranEViewsUninitialize()
   
      call Uninitialize()
   
   end subroutine FortranEViewsUninitialize

!==============================================================================

   subroutine Show()
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Show(app)
   
   end subroutine Show
   
!==============================================================================

   subroutine Hide()
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Hide(app)
   
   end subroutine Hide

!==============================================================================
   
   subroutine ShowLog()
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_ShowLog(app)
   
   end subroutine ShowLog
   
!==============================================================================

   subroutine HideLog()
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_HideLog(app)
   
   end subroutine HideLog
   
!==============================================================================
   
   subroutine Run(commandString)
   
      character (len=*) :: commandString
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Run(app, commandString)
   
   end subroutine Run
   
!==============================================================================   

   subroutine Lookup(patternString, typeString, returnType, names)
   
      character (len=*) :: patternString, typeString
      integer :: returnType
      type (variant) :: names
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Lookup(app, patternString, typeString, returnType, names)
   
   end subroutine Lookup

!==============================================================================
   
   subroutine ListToArray(nameString, nameArray)
   
      character (len=*) :: nameString
      type (variant) :: nameArray
      
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_ListToArray(app, nameString, nameArray)
   
   end subroutine ListToArray
   
!==============================================================================
   
   subroutine ArrayToList(nameArray, nameString)
   
      type (variant) :: nameArray
      character (len=*) :: nameString
      
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_ArrayToList(app, nameArray, nameString)
   
   end subroutine ArrayToList
   
!==============================================================================
   
   subroutine Get(objectName, naType, naString, objectData)
   
   
      character (len=*) :: objectName, naString
      integer :: naType
      type (variant) :: objectData
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Get(app, objectName, naType, naString, objectData)
   
   end subroutine Get

!==============================================================================
   
      subroutine Get2D(objectName, naType, naString, objectData)
   
   
      character (len=*) :: objectName, naString
      integer :: naType
      type (variant) :: objectData
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication8_Get2D(app, objectName, naType, naString, objectData)
   
   end subroutine Get2D

!==============================================================================
   
   subroutine GetSeries(seriesName, sampleString, naType, naString, seriesData)
   
   
      character (len=*) :: seriesName, sampleString, naString
      integer :: naType
      type (variant) :: seriesData
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_GetSeries(app, seriesName, sampleString, naType, naString, seriesData)
   
   end subroutine GetSeries

!==============================================================================
   
   subroutine GetSeries2D(seriesName, sampleString, naType, naString, seriesData)
   
      character (len=*) :: seriesName, sampleString, naString
      integer :: naType
      type (variant) :: seriesData
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication8_GetSeries2D(app, seriesName, sampleString, naType, naString, seriesData)
   
   end subroutine GetSeries2D

!==============================================================================
 
   subroutine GetGroup(seriesNames, sampleString, naType, naString, seriesData)
      
      type (variant) :: seriesNames, seriesData
      character (len=*) :: sampleString, naString
      integer :: naType
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_GetGroup(app, seriesNames, sampleString, NAType, naString, seriesData)
   
   end subroutine GetGroup

!==============================================================================
   
   subroutine GetGroupEx(seriesNames, sampleString, naType, naString, groupOptions, seriesData)
      
      type (variant) :: seriesNames, seriesData
      character (len=*) :: sampleString, naString, groupOptions
      integer :: naType
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_GetGroupEx(app, seriesNames, sampleString, NAType, naString, groupOptions, seriesData)
   
   end subroutine GetGroupEx

!==============================================================================
   
   subroutine Put(objectName, objectData, DataType, WriteType)
   
      character (len=*) :: objectName
      type (variant) :: objectData
      integer :: DataType, WriteType
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Put(app, objectName, objectData, DataType, WriteType)
   
   end subroutine Put

!==============================================================================     
   
   subroutine PutSeries(seriesName, seriesData, sampleString, SeriesType, WriteType)
   
      character (len=*) :: seriesName, sampleString
      integer :: SeriesType, WriteType
      type (variant) :: seriesData
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_PutSeries(app, seriesName, seriesData, sampleString, SeriesType, WriteType)
   
   end subroutine PutSeries

!==============================================================================      
   
   subroutine PutGroup(seriesNames, seriesData, sampleString, SeriesType, WriteType)
   
      type (variant) :: seriesNames, seriesData
      character (len=*) :: sampleString
      integer :: SeriesType, WriteType
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_PutGroup(app, seriesNames, seriesData, sampleString, SeriesType, WriteType)
   
   end subroutine PutGroup

!==============================================================================      
   
   subroutine Initialize()
   
      if (.not. initialized) then
         call COMInitialize(status)
         call COMCreateObject("Eviews.Manager", mgr, status)
         status = $IManager_GetApplication(mgr, NewInstance, .false., app)
         ! Cast to type IApplication9 to gain access to Get2D and GetSeries2D subroutines
         ! call COMGetActiveObjectByGUID(CLSID_Application, IID_IApplication9, app, status)
         ! status = $IApplication9_SetEventTypeMask(app,  EViewsEventTypeNone)
         initialized = .true.
      end if

   end subroutine Initialize
   
!==============================================================================
   
   subroutine Uninitialize()

      if (initialized) then
         status = $IApplication_Hide(app)
         status = COMReleaseObject(app)
         status = COMReleaseObject(mgr)
         call COMUninitialize()
         initialized = .false.
      end if

   end subroutine Uninitialize
   
!==============================================================================
   
   subroutine FortranEViewsTest
   
      integer :: i
      integer (int_ptr_kind()) :: value, lbound, ubound, yptr
      real (kind=8), dimension(2) :: series = (/ 1.2d0, 2.3d0 /)
      type (variant) :: x, y, element, names, array, bstr
      type (sa_bounds) :: bounds
   
      !DEC$ IF (.FALSE.)
      write(*,'(a/)') "   Show()"
      call Show()
      read(*,*)
   
      write(*,'(a/)') "   Hide()"
      call Hide()
      read(*,*)
   
      write(*,'(a/)') "   ShowLog()"
      call ShowLog()
      call Show()
      read(*,*)
   
      write(*,'(a/)') "   HideLog()"
      call HideLog()
      read(*,*)
   
      write(*,'(a/)') '   Run("wfopen c:\mywork.wf1")'
      call Run("wfopen c:\mywork.wf1")
      read(*,*)

      write(*,'(a/)') '   Lookup("s* g*", "series", LookupReturnString)'
      call Lookup("s* g*", "series", LookupReturnString, names)
      status = ConvertBSTRToString(names%vu%ptr_val, output)
      write(*,'("      ",a)') output(1:status)
      read(*,*)
      
      !DEC$ END IF
      
      call Run("wfopen c:\mywork.wf1")
   
      write(*,'(a/)') '   GetSeries("x", "2000 2016", NATypeAsEmpty, "",x )'
      call GetSeries("x", "2000 2016", NATypeAsEmpty, "", x)
   
      !status = SafeArrayGetLBound(%VAL(x%vu%ptr_val), 1, lbound)
      !status = SafeArrayGetUBound(%VAL(x%vu%ptr_val), 1, ubound)
   
      do i = lbound, ubound
         status = SafeArrayGetElement(%VAL(x%vu%ptr_val),i,%LOC(element))
         write(*,'(i10,f10.3)') 2000+i, element%vu%double_val
      end do
   
      !read(*,*)
   
      write(*,'(a/)') '   PutSeries("y", y, "2008 2009", SeriesTypeAuto, WriteUpdate)'
      bounds%extent = 2
      bounds%lbound = 0
      yptr = SafeArrayCreate(VT_VARIANT, 1, bounds)
      y%vt = OR(VT_ARRAY,VT_VARIANT)
      y%vu%ptr_val = yptr
   
      do i = 1, size(series)
         element%vt = VT_R8
         element%vu%double_val = series(i)
         status = SafeArrayPutElement(%VAL(y%vu%ptr_val), i-1, %LOC(element))
         write(*,'(i10,f10.3)') 2008 + (i-1), series(i)
      end do
   
      !read(*,*)
   
      call PutSeries("y", y, "2008 2009", SeriesTypeSeries, WriteUpdate)
   
      call Run("wfsave(2) c:\mywork.wf1")
      call Run("wfclose c:\mywork.wf1")
      
      call Run("wfopen c:\mywork.wf1")
      call Run('string names = @wquery("otl_tr162","name matches *.a and type=series","name")')
      call Get("names", NATypeAsEmpty, "", names)
      status = ConvertBSTRToString(names%vu%ptr_val, output)
      !call ListToArray('ADJ2013.A ADJ2013B.A',array)
      call ListToArray(output(1:500000),array)
      !status = SafeArrayGetLBound(%VAL(array%vu%ptr_val), 1, lbound)
      !status = SafeArrayGetUBound(%VAL(array%vu%ptr_val), 1, ubound)
      status = SafeArrayGetDim(%VAL(array%vu%ptr_val))
      do i = lbound, ubound
         status = SafeArrayGetElement(%VAL(array%vu%ptr_val),i,%LOC(element))
         status = ConvertBSTRToString(element%vu%ptr_val, output)
         !write(*,'(a)') output(1:status)
         seriesNames(i+1) = output(1:status)
      end do
      write(*,'(i8, a)') i+1, output(1:status)
      call Run("wfsave(2) c:\mywork.wf1")
      call Run("wfclose c:\mywork.wf1")
   
   end subroutine FortranEViewsTest
   
!==============================================================================
   
   subroutine OpenEViewsWorkfile(fileName)
   
      character (len=*) :: fileName
   
      if (.not. initialized) call FortranEViewsInitialize()
      call Run("wfopen "//trim(fileName)//"")
      if (status /= 0) then
         write(*,'(a)') " Error: Workfile "//trim(fileName)//" cannot be opened"
         call Uninitialize()
         stop
      end if
   
   end subroutine OpenEViewsWorkfile
   
!==============================================================================
   
   subroutine CloseEViewsWorkfile(name, wfsave)
   
      character (len=*) :: name
      logical, optional :: wfsave
   
      if (.not. initialized) call FortranEViewsInitialize()
      call Run("wfselect "//trim(name)//"")
      if (.not. present(wfsave)) then ! default behavior is to save
         call Run("wfsave(2) "//trim(name)//"")
      else if (wfsave == .true.) then ! explicit save argument
         call Run("wfsave(2) "//trim(name)//"")
      end if
      ! specifying .false. will skip save
      call Run("wfclose "//trim(name)//"")
   
   end subroutine
   
!==============================================================================
   
   subroutine FetchEViewsSeries(workfile, seriesName, dataArray)
   
      character (len=*) :: workfile, seriesName
      real (kind = 8), dimension(:) :: dataArray
      type (variant) :: series, element, index
      integer :: i
      
      if (.not. initialized) call FortranEViewsInitialize()
      
      call Run("wfselect "//trim(workfile)//"")
      
      if (size(dataArray) == 210) then
         call Run("pageselect a")
      else if (size(dataArray) == 843) then
         call Run("pageselect q")
      else if (size(dataArray) == 2531) then
         call Run("pageselect m")
      else
         write(*,*) "Warning: Array size is incorrect for series "//trim(seriesName)
         return
      end if
      
      ! call Show()
      ! call ShowLog()
      
      call Run("scalar per1 = @ifirst("//trim(seriesName)//")")
      call Run("scalar per2 = @ilast("//trim(seriesName)//")")

      call Get("per1", NATypeAsEmpty, "", index)
      per1 = nint(index%vu%double_val)
      call Get("per2", NATypeAsEmpty, "", index)
      per2 = nint(index%vu%double_val)      
      
      call GetSeries(trim(seriesName), "@all", NATypeAsEmpty, "", series)
      
      if (status /= 0) then
          write(*,*) "Warning: Series "//trim(seriesName)//" not found in "//trim(workfile)
         return
      end if

      do i = per1, per2
         status = SafeArrayGetElement(%VAL(series%vu%ptr_val),i-1,%LOC(element))
         dataArray(i) = element%vu%double_val
      end do
      
      ! Don't call destructor
      ! status = SafeArrayDestroy(%VAL(series%vu%ptr_val))
      
      call Run("delete per1 per2")
      
   end subroutine FetchEViewsSeries
   
!==============================================================================
   
   subroutine FetchEViewsSeriesVersion(workfile, seriesName, version, dataArray)
   
      character (len=*) :: workfile, seriesName, version
      real (kind = 8), dimension(:) :: dataArray
      type (variant) :: series, element, index
      integer :: i
      
      if (.not. initialized) call FortranEViewsInitialize()
      
      call Run("wfselect "//trim(workfile)//"")
      
      call Run("pageselect "//trim(version)//"")
      
      ! call Show()
      ! call ShowLog()
      
      call Run("scalar per1 = @ifirst("//trim(seriesName)//")")
      call Run("scalar per2 = @ilast("//trim(seriesName)//")")

      call Get("per1", NATypeAsEmpty, "", index)
      per1 = nint(index%vu%double_val)
      call Get("per2", NATypeAsEmpty, "", index)
      per2 = nint(index%vu%double_val)      
      
      call GetSeries(trim(seriesName), "@all", NATypeAsEmpty, "", series)
      
      if (status /= 0) then
          write(*,*) "Warning: Series "//trim(seriesName)//"."//trim(version)//" not found in "//trim(workfile)
         return
      end if

      do i = per1, per2
         status = SafeArrayGetElement(%VAL(series%vu%ptr_val),i-1,%LOC(element))
         dataArray(i) = element%vu%double_val
      end do
      
      ! Don't call destructor
      ! status = SafeArrayDestroy(%VAL(series%vu%ptr_val))
      
      call Run("delete per1 per2")
      
   end subroutine FetchEViewsSeriesVersion
   
!==============================================================================

   subroutine StoreEViewsSeries(workfile, seriesName, dataArray)
   
      character (len=*) :: workfile, seriesName
      real (kind = 8), dimension(:) :: dataArray
      type (variant) :: series, element, index
      integer :: i
      type (sa_bounds) :: bounds
      integer (int_ptr_kind()) :: yptr
      type (variant) :: y
      character (len=256) :: smpl
      
      if (.not. initialized) call FortranEViewsInitialize()
      
      call Run("wfselect "//trim(workfile)//"")
      
      if (size(dataArray) == 210) then
         call Run("pageselect a")
         smpl = "1901 2105"
      else if (size(dataArray) == 843) then
         call Run("pageselect q")
         smpl = "1900q2 2105q4"
      else if (size(dataArray) == 2531) then
         call Run("pageselect m")
         smpl = "1900m2 2105m12"
      else
         write(*,*) "Warning: Array size is incorrect for series "//trim(seriesName)
         return
      end if
   
      bounds%extent = size(dataArray)
      bounds%lbound = 0
      yptr = SafeArrayCreate(VT_VARIANT, 1, bounds)
      y%vt = OR(VT_ARRAY,VT_VARIANT)
      y%vu%ptr_val = yptr
   
      !do i = 10, size(dataArray)
      do i = per1, per2
         element%vt = VT_R8
         element%vu%double_val = dataArray(i)
         status = SafeArrayPutElement(%VAL(y%vu%ptr_val), i-1, %LOC(element))
      end do
   
      call PutSeries(trim(seriesName), y, trim(smpl), SeriesTypeSeries, WriteUpdate)

   end subroutine StoreEViewsSeries
   
!==============================================================================
   
   subroutine StoreEViewsSeriesVersion(workfile, seriesName, version, dataArray)
   
      character (len=*) :: workfile, seriesName, version
      real (kind = 8), dimension(:) :: dataArray
      type (variant) :: series, element, index
      integer :: i
      type (sa_bounds) :: bounds
      integer (int_ptr_kind()) :: yptr
      type (variant) :: y
      character (len=256) :: smpl
      integer :: Annual
      
      if (.not. initialized) call FortranEViewsInitialize()
      
      call Run("wfselect "//trim(workfile)//"")
      
      call Run("pageselect "//trim(version)//"")
      
      if (size(dataArray) == 210) then
         smpl = "1901 2105"
      else if (size(dataArray) == 843) then
         smpl = "1900q2 2105q4"
      else if (size(dataArray) == 2531) then
         smpl = "1900m2 2105m12"
      else
         write(*,*) "Warning: Array size is incorrect for series "//trim(seriesName)
         return
      end if
   
      bounds%extent = size(dataArray)
      bounds%lbound = 0
      yptr = SafeArrayCreate(VT_VARIANT, 1, bounds)
      y%vt = OR(VT_ARRAY,VT_VARIANT)
      y%vu%ptr_val = yptr
   
      !do i = 10, size(dataArray)
      do i = per1, per2
         element%vt = VT_R8
         element%vu%double_val = dataArray(i)
         status = SafeArrayPutElement(%VAL(y%vu%ptr_val), i-1, %LOC(element))
      end do
   
      call PutSeries(trim(seriesName), y, trim(smpl), SeriesTypeSeries, WriteUpdate)
      
   end subroutine StoreEViewsSeriesVersion
   
!==============================================================================
   
   subroutine GetEViewsString(workfile, listName, string)
   
      character (len=*) :: workfile, listName
      type (variant) :: listString
      character (len=*) :: string
   
      if (.not. initialized) call FortranEViewsInitialize()
      
      call Run("wfselect "//trim(workfile)//"")
      call Run("pageselect vars")
      call Get(trim(listName), NATypeAsEmpty, "", listString)
      status = ConvertBSTRToString(listString%vu%ptr_val, string)
  
   end subroutine GetEViewsString
   
!==============================================================================   
   
   subroutine CreateEViewsWorkfile(fileName)
   
      character (len=*) :: fileName
   
      if (.not. initialized) call FortranEViewsInitialize()
      call Run("wfcreate(wf="//trim(fileName)//", page=a) a 1901 2105")
      call Run("pagecreate(page=q) q 1900Q2 2105Q4")
      call Run("pagecreate(page=m) m 1900M2 2105M12")
      call Run("pagecreate(page=vars) a 1901 2105")
      call Run("wfsave(2) "//trim(fileName)//"")
      call Run("wfclose "//trim(fileName)//"")
   
   end subroutine CreateEViewsWorkfile
   
!==============================================================================
   
   function ImportModelSolution(fileName) result(rv)
   
      character (len=*) :: fileName
      integer :: rv
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Run(app, "exec " // trim(fileName))
      rv = status
   
   end function ImportModelSolution
   
!==============================================================================
   
   function ExecuteEViewsProgram(fileName) result(rv)
   
      character (len=*) :: fileName
      integer :: rv
   
      if (.not. initialized) call FortranEViewsInitialize()
      status = $IApplication_Run(app, "exec " // trim(fileName))
      rv = status
   
   end function ExecuteEViewsProgram
   
!==============================================================================   
   
end module FortranEViewsModEx
