subroutine start_irrig_auto(n,nplt,nger,nlev,namf,nlax,ndrp,nflo,nsen,nrec,nmat,ndebdorm,nfindorm,  &
                     &         stage_start_irrigauto,stage_end_irrigauto,n_datedeb_irrigauto,n_datefin_irrigauto)

  implicit none

integer :: nplt,nger,nlev,namf,nlax,ndrp,nflo,nsen,nrec,nmat,ndebdorm,nfindorm
integer :: n_datedeb_irrigauto,n_datefin_irrigauto,n
character*3 :: stage_start_irrigauto,stage_end_irrigauto


! calcul du debut
              if ((nplt == n) .and. &
              (stage_start_irrigauto.eq.'plt'.or.stage_start_irrigauto.eq.'PLT')) n_datedeb_irrigauto=n
              if ((nger == n) .and. &
              (stage_start_irrigauto.eq.'ger'.or.stage_start_irrigauto.eq.'GER')) n_datedeb_irrigauto=n
              if ((nlev == n) .and. (stage_start_irrigauto.eq.'lev'.or.stage_start_irrigauto.eq.'LEV')) n_datedeb_irrigauto=n
              if ((namf == n) .and. (stage_start_irrigauto.eq.'amf'.or.stage_start_irrigauto.eq.'AMF')) n_datedeb_irrigauto=n
              if ((nlax == n) .and. (stage_start_irrigauto.eq.'lax'.or.stage_start_irrigauto.eq.'LAX')) n_datedeb_irrigauto=n
              if ((ndrp == n) .and. (stage_start_irrigauto.eq.'drp'.or.stage_start_irrigauto.eq.'DRP')) n_datedeb_irrigauto=n
              if ((nflo == n) .and. (stage_start_irrigauto.eq.'flo'.or.stage_start_irrigauto.eq.'FLO')) n_datedeb_irrigauto=n
              if ((nsen == n) .and. (stage_start_irrigauto.eq.'sen'.or.stage_start_irrigauto.eq.'SEN')) n_datedeb_irrigauto=n
              if ((nrec == n) .and. (stage_start_irrigauto.eq.'rec'.or.stage_start_irrigauto.eq.'REC')) n_datedeb_irrigauto=n
              if ((nmat == n) .and. (stage_start_irrigauto.eq.'mat'.or.stage_start_irrigauto.eq.'MAT')) n_datedeb_irrigauto=n
              if ((ndebdorm == n) .and. &
              (stage_start_irrigauto.eq.'debdorm'.or.stage_start_irrigauto.eq.'DEBDORM')) n_datedeb_irrigauto=n
              if ((nfindorm == n) .and. &
              (stage_start_irrigauto.eq.'findorm'.or.stage_start_irrigauto.eq.'FINDORM')) n_datedeb_irrigauto=n
! calcul de la fin
              if ((nplt == n) .and. (stage_end_irrigauto.eq.'plt'.or.stage_end_irrigauto.eq.'PLT')) n_datefin_irrigauto=n
              if ((nger == n) .and. (stage_end_irrigauto.eq.'ger'.or.stage_end_irrigauto.eq.'GER')) n_datefin_irrigauto=n
              if ((nlev == n) .and. (stage_end_irrigauto.eq.'lev'.or.stage_end_irrigauto.eq.'LEV')) n_datefin_irrigauto=n
              if ((namf == n) .and. (stage_end_irrigauto.eq.'amf'.or.stage_end_irrigauto.eq.'AMF')) n_datefin_irrigauto=n
              if ((nlax == n) .and. (stage_end_irrigauto.eq.'lax'.or.stage_end_irrigauto.eq.'LAX')) n_datefin_irrigauto=n
              if ((ndrp == n) .and. (stage_end_irrigauto.eq.'drp'.or.stage_end_irrigauto.eq.'DRP')) n_datefin_irrigauto=n
              if ((nflo == n) .and. (stage_end_irrigauto.eq.'flo'.or.stage_end_irrigauto.eq.'FLO')) n_datefin_irrigauto=n
              if ((nsen == n) .and. (stage_end_irrigauto.eq.'sen'.or.stage_end_irrigauto.eq.'SEN')) n_datefin_irrigauto=n
              if ((nrec == n) .and. (stage_end_irrigauto.eq.'rec'.or.stage_end_irrigauto.eq.'REC')) n_datefin_irrigauto=n
              if ((nmat == n) .and. (stage_end_irrigauto.eq.'mat'.or.stage_end_irrigauto.eq.'MAT')) n_datefin_irrigauto=n
              if ((ndebdorm == n) .and. &
              (stage_end_irrigauto.eq.'debdorm'.or.stage_end_irrigauto.eq.'DEBDORM')) n_datefin_irrigauto=n
              if ((nfindorm == n) .and. &
              (stage_end_irrigauto.eq.'findorm'.or.stage_end_irrigauto.eq.'FINDORM')) n_datefin_irrigauto=n

return
end subroutine start_irrig_auto
