module WorkfileInterface

   use ifport
   use FortranEViewsModEx
   include "OcactFortLib.inc"
   implicit none
   
   private
   
   logical :: workfileVerboseMode
   integer, dimension(2) :: sample
   
   integer, save :: workfileNum = 1
   character (len = 256), dimension(64) :: workfile
   character (len = 256), dimension(64) :: workfileName
   
   public :: workfileVerboseMode, sample
   public :: SetWorkfileVerboseMode, GetSample
   public :: OpenWorkfile, CloseWorkfile
   public :: FetchSeries, StoreSeries
   public :: FetchList
   public :: CreateWorkfile
   public :: ExecuteProgram
   public :: MAX_YR, MAX_QTR, MAX_MTH
   public :: MAX_LIST_SIZE, LIST_ITEM_LEN

   contains
   
!===============================================================================
   
   subroutine SetWorkfileVerboseMode(setting)
   
       logical :: setting
       
       workfileVerboseMode = setting
       if (setting == .false.) call FortranEViewsUninitialize()
       
   end subroutine SetWorkfileVerboseMode
   
!===============================================================================   
   
   function GetSample() result(rv)
   
      integer, dimension(2) :: rv
      
      rv = sample
      
   end function GetSample
   
!===============================================================================   
   
   function OpenWorkfile(fileName) result(fileNum)
   
      character (len=*) :: fileName
      integer :: fileNum, rv
      character (len = 256) :: path, drive, dir, name, ext
      
      fileNum = workfileNum
      workfile(fileNum) = trim(fileName)
      path = trim(workfile(fileNum))
      rv = splitpathqq(path,drive,dir,name,ext)
      workfileName(fileNum) =  trim(name)
      call OpenEViewsWorkfile(trim(drive)//trim(dir)//trim(name)//trim(ext))
      workfileNum = workfileNum + 1
      
   end function OpenWorkfile
   
!===============================================================================   
   
   subroutine CloseWorkfile(fileNum, wfsave)
   
      integer :: fileNum
      logical, optional :: wfsave
      
      if (.not. present(wfsave)) then
         call CloseEViewsWorkfile(trim(workfile(fileNum)))
      else
         call CloseEViewsWorkfile(trim(workfile(fileNum)), wfsave)
      end if
      
   end subroutine CloseWorkfile
   
!===============================================================================   
   
   subroutine FetchSeries(fileNum, seriesName, dataArray)
   
      integer :: fileNum
      character (len=*) :: seriesName
      real (kind = 8), dimension(:) :: dataArray
      integer :: dot
      character (len=256) :: shortSeriesName, version
      
      dot = scan(seriesName,'.') ! allow optional version (frequency) specification for backwards compatibility
      if (dot == 0) then ! size of array determines workfile page
         call FetchEViewsSeries(trim(workfileName(fileNum)), seriesName, dataArray)
      else
         shortSeriesName = trim(seriesName(1:scan(seriesName,'.')-1))
         version = seriesName(scan(seriesName,'.')+1:)
                 ! annual, quarterly, or monthly as version
         if (trim(version) == 'A' .or. trim(version) == 'Q' .or. trim(version) == 'M' .or.&
            trim(version) == 'a' .or. trim(version) == 'q' .or. trim(version) == 'm') then
               call FetchEViewsSeries(trim(workfileName(fileNum)), trim(shortSeriesName), dataArray)
            else ! custom version such as .add or .exg
               call FetchEViewsSeriesVersion(trim(workfileName(fileNum)), trim(shortSeriesName), trim(version), dataArray) 
         end if
      end if
      sample(1) = per1
      sample(2) = per2
      
   end subroutine FetchSeries
   
!===============================================================================
   
   subroutine StoreSeries(fileNum, seriesName, dataArray)
   
      integer :: fileNum
      character (len=*) :: seriesName
      real (kind = 8), dimension(:) :: dataArray
      integer :: dot
      character (len=256) :: shortSeriesName, version
      
      per1 = sample(1)
      per2 = sample(2)
      
      dot = scan(seriesName,'.') ! allow optional version (pagename)
      if (dot == 0) then ! size of array determines workfile page
         call StoreEViewsSeries(trim(workfileName(fileNum)), seriesName, dataArray)
      else
         shortSeriesName = trim(seriesName(1:scan(seriesName,'.')-1))
         version = seriesName(scan(seriesName,'.')+1:)
         ! annual, quarterly, or monthly as version
         if (trim(version) == 'A' .or. trim(version) == 'Q' .or. trim(version) == 'M' .or.&
            trim(version) == 'a' .or. trim(version) == 'q' .or. trim(version) == 'm') then
             call StoreEViewsSeries(trim(workfileName(fileNum)), trim(shortSeriesName), dataArray)
         else ! custom pagename such as Annual
             call StoreEViewsSeriesVersion(trim(workfileName(fileNum)), trim(shortSeriesName), trim(version), dataArray)
         endif
      end if
      
   end subroutine StoreSeries
   
!===============================================================================
   
   subroutine FetchList(fileNum, listName, listArray)
   
      integer :: fileNum
      character (len=*) :: listName
      character (len=256), dimension(0:2000) :: listArray
      character (len=2000*256*2) :: string
      integer :: i, j, k, lenString
      character (len=256) :: item
      logical :: inQuote
      
      call GetEViewsString(trim(workfileName(fileNum)), listName, string)
      ! Tokenize the string, populate the listArray, and add the size to element 0
      item = ""
      j = 1
      k = 0
      inQuote = .false.
      listArray(0) = '0'
      lenString = len_trim(string)
      do i = 1, lenString
         if (string(i:i) == '"') inQuote = .not. inQuote
         if (string(i:i) /= ' ' .and. i /= lenString .or. inQuote) then
            item(j:j) = string(i:i)
            j = j + 1
         else
            if (i == lenString) item(j:j) = string(i:i)
            j = 1
            k = k + 1
            inQuote = .false.
            listArray(k) = trim(item)
            listArray(0) = trim(IntToAsc(k))
            item = ""
            cycle
         end if
      end do
      
   end subroutine FetchList
   
!===============================================================================      
   
   subroutine CreateWorkfile(fileName)
   
      character (len=*) :: fileName
   
      call CreateEViewsWorkfile(fileName)
   
   end subroutine CreateWorkfile
   
!===============================================================================   
   
   function ExecuteProgram(fileName) result (rv)
   
      character (len=*) :: fileName
      integer :: rv
      
      rv = ExecuteEViewsProgram(trim(fileName))
      
   end function ExecuteProgram
      
!===============================================================================      
   
end module WorkfileInterface   