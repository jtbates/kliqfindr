      subroutine binomial_coef ( n, k, cnk )

        integer cnk
        integer i
        integer k
        integer mn
        integer mx
        integer n

        mn = min ( k, n-k )

        if ( mn .lt. 0 ) then

          cnk = 0

        else if ( mn .eq. 0 ) then

          cnk = 1

        else

          mx = max ( k, n-k )
          cnk = mx + 1

          do i = 2, mn
            cnk = ( cnk * ( mx + i ) ) / i
          end do

        end if

        return
      end
