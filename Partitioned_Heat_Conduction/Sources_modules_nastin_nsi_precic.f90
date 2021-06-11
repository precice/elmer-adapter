subroutine nsi_precic(itask)
  !-----------------------------------------------------------------------
  !****f* nastin/nsi_precic
  ! NAME
  !   nsi_precic
  ! DESCRIPTION
  !    Calls the preCICE library calls. For more information please refer to
  !    http://www5.in.tum.de/wiki/index.php/PreCICE_Webpage  or
  !    uekerman@in.tum.de .
  !    The workflow is organised via Task-IDs:
  !    ITASK = 1 ... Create preCICE
  !          = 2 ... Initialize preCICE
  !          = 3 ... Advance preCICE
  !          = 4 ... Finalize preCICE
  !          = 5 ... sets time step length
  ! OUTPUT
  ! USES
  ! USED BY
  !    nsi_turnon
  !    nsi_iniunk
  !    nsi_concou
  !    nsi_turnof
  !    nsi_timste
  !***
  !-----------------------------------------------------------------------

  !TODO einschränken!
  use def_kintyp
  use def_master, only : dtime, bvess_ale, press, veloc, kfl_preci, gevec, &
                         pre_idF, pre_idDD, pre_nvert, pre_tsl, pre_idMes, pre_codno, &
                         pre_vertIDs, pre_index, kfl_gocou, pre_final, &
                         INOTMASTER, npoi1, npoi2, npoi3, pre_excha, NPOIN_TYPE
  use def_domain, only : npoin, coord, kfl_codno, r_dom, c_dom, ndime, lpoty
  use def_nastin, only : kfl_intfo_nsi, intfo_nsi
  use def_parall, only : nproc_par, iproc_par !TODO at the moment, you need to compile parall
  use def_alefor, only : coord_ale

  implicit none
  integer(ip), intent(in)    :: itask ! task to pass workflow to this module
  integer(ip)                :: ipoin,idime,iposs,iinde,izdom,jzdom,jpoin,jdime
  real(rp),    pointer       :: positions(:), preF(:), preDD(:) ! preCICE data structures
  character*50               :: coric, cowic ! constants, read/write-iteration/sim-checkpoint
  integer(ip)                :: kfl_required ! 1: yes, 0: no


  kfl_preci = 1
  pre_codno = 20
  if( kfl_preci==1) then  !precice used,

     select case(itask)

     case(1_ip)
        !
        ! Create preCICE, called by nsi_turnon
        !
        print *, "PRECICE create"
        call precicef_create("NASTIN", "../precice-config.xml", iproc_par, nproc_par)
        
        pre_final = 0

     case(2_ip)
        !
        ! Initialize preCICE, called by nsi_iniunk
        !
        print *, "PRECICE initialize"

        call precicef_get_mesh_id("NASTIN_Mesh", pre_idMes )
        call precicef_get_data_id("Forces", pre_idMes, pre_idF)
        call precicef_get_data_id("Displacements", pre_idMes, pre_idDD)

        !figure out how many nodes are locate at the wet surface
        pre_nvert=0_ip
        if(INOTMASTER) then
          do ipoin = 1,npoin
            if( kfl_codno(1_ip,ipoin)==pre_codno .or. kfl_codno(2_ip,ipoin)==pre_codno ) then
                pre_nvert = pre_nvert + 1_ip  
            end if
          end do
        end if

        if (pre_nvert==0) then !i.e. is processor not on WetSurface?
          pre_excha = 0
        else
          pre_excha = 1
        end if

        print *, "PRECICE: Nodes on WetSurface: ", pre_nvert, ", pro: ", iproc_par

        !allocate all data structures needed in this module
        if(pre_excha==1) then

          allocate(positions(ndime*pre_nvert))
          allocate(pre_vertIDs(pre_nvert))
          allocate(pre_index(pre_nvert))

          !collect positions and indeces of the nodes located at the wet surface

          !write positions
          iposs=1_ip
          iinde=1_ip
          do ipoin = 1,npoin
            if( kfl_codno(1_ip,ipoin)==pre_codno .or. kfl_codno(2_ip,ipoin)==pre_codno) then
              !if (ipoin<=npoi1 .or. (ipoin>=npoi2 .and. ipoin<=npoi3)) then
                do idime = 1,ndime
                  positions(iposs) = coord(idime,ipoin)
                  iposs = iposs + 1_ip
                end do
                pre_index(iinde) = ipoin
                iinde = iinde + 1_ip
              !end if
            end if
          end do

          ! give positions to precice
          call precicef_set_vertices(pre_idMes, pre_nvert, positions, pre_vertIDs)
print *, iproc_par, " ", positions
          deallocate(positions)
        
        end if! pre_excha

        ! adaptive tsl should already be calculated here by Alya
        call precicef_initialize(pre_tsl)

        print *, "PRECICE initialized"


     case(3_ip)
        !
        ! Advance preCICE, called by nsi_advance
        !

        print *, "PRECICE advance"

        ! strings have to be prefilled with blancs to be compatible to preCICE
        cowic(1:50)='                                                  '
        coric(1:50)='                                                  '


        call precicef_action_write_iter_checkp(cowic)
        call precicef_action_required(cowic,kfl_required)

        if (kfl_required==1) then !i.e. checkpointing required
          call precicef_mark_action_fulfilled(cowic)
          !nothing to do here at the moment
          kfl_required = -1
        end if

        if(INOTMASTER) then
          call memgen(0_ip,ndime,npoin)
        end if

        if(pre_excha==1) then
        
          allocate(preF(ndime*pre_nvert))
          allocate(preDD(ndime*(pre_nvert)))

          if(kfl_intfo_nsi/=2_ip) then
            call runend('NSI_PRECIC: TO COMMUNICATE FORCES WITH PRECICE YOU NEED RESIDUAL FORCES')
          end if


          !calculate residual forces
          do ipoin=1,npoin
            if(lpoty(ipoin)>0) then
              do idime = 1,ndime
                gevec(idime,ipoin) = intfo_nsi(ipoin) % bu(idime)
                jzdom = 0
                do izdom = r_dom(ipoin),r_dom(ipoin+1) - 1
                  jpoin = c_dom(izdom)
                  jzdom = jzdom + 1
                  do jdime = 1,ndime
                    gevec(idime,ipoin) = gevec(idime,ipoin) - intfo_nsi(ipoin) % Auu(jdime,idime,jzdom) * veloc(jdime,jpoin,1)
                  end do
                  gevec(idime,ipoin) = gevec(idime,ipoin) - intfo_nsi(ipoin) % Aup(idime,jzdom) * press(jpoin,1)
                end do
              end do
            end if
          end do

        end if !pre_exchange


        if(pre_excha==1) then

          !copy forces to preCICE data structure
          do iinde = 1_ip,pre_nvert
            ipoin = pre_index(iinde)
            do idime = 1,ndime
              iposs = (iinde-1)*ndime+idime
              preF(iposs) = gevec(idime,ipoin)
            end do
          end do
          
          ! write forces to preCICE
          call precicef_write_bvdata(pre_idF,pre_nvert,pre_vertIDs,preF)

        end if !pre_excha


        pre_tsl = dtime
        call precicef_advance(pre_tsl)
        print *, "PRECICE advanced, tsl: ", pre_tsl

        if(pre_excha==1) then

          ! read displacement deltas from preCICE
          call precicef_read_bvdata(pre_idDD,pre_nvert,pre_vertIDs,preDD)            

          ! copy displacement deltas from preCICE data structure to Alya data structure
          do iinde = 1,pre_nvert
            do idime = 1,ndime
              ipoin = pre_index(iinde)
              bvess_ale(idime,ipoin)=preDD((iinde-1)*ndime+idime)
            end do
          end do

          deallocate(preF)
          deallocate(preDD)

        end if! pre_excha

        ! TODO, do not use gevec here, not readable
        if(INOTMASTER) then
          call memgen(2_ip,ndime,npoin)
        end if


        call precicef_action_read_iter_checkp(coric)
        call precicef_action_required(coric,kfl_required)

        if (kfl_required==1) then !i.e. not yet converged
          print *, "PRECICE, not converged"
          kfl_gocou=1 !here we use the coupling loop
          call precicef_mark_action_fulfilled(coric)
          kfl_required = -1

        else !i.e. converged
          print *, "PRECICE converged, advancing in time"
          ! move mesh
          if(INOTMASTER) then
            do ipoin = 1, npoin 
              do idime = 1, ndime
                coord(idime,ipoin) =  coord_ale(idime,ipoin,1)
              end do
            end do
          end if

        end if

        call precicef_ongoing(kfl_required)
        if (kfl_required==0) then 
          print *, "PRECICE asks to shut down"
          pre_final = 1
        end if 

     case(4_ip)
        !
        ! Finalize preCICE, called by nsi_turnof
        !
        print *, "PRECICE finalize"

        if(pre_excha==1) then
          !deallocate all data structure of this module
          deallocate(pre_vertIDs)
          deallocate(pre_index)
        end if

        cowic(1:50)='                                                  '
        call precicef_action_write_iter_checkp(cowic)
        call precicef_action_required(cowic,kfl_required)
        if (kfl_required==1) then !i.e. checkpointing required
          call precicef_mark_action_fulfilled(cowic)
          kfl_required = -1
        end if

        call precicef_finalize()

    case(5_ip)
        !
        ! set time step length, called by nsi_timste
        !
        !TODO not yet working
        !TODO also change dtinv!!!
        dtime = min(dtime, pre_tsl)

    end select

  end if  !precice defined

end subroutine nsi_precic
