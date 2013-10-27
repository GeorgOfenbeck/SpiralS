/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ch.ethz.spirals.validation

import java.nio.IntBuffer
import com.sun.jna.NativeLong

object fftw3j_1D {
  import fftw3.{FFTW3Library => FFTW}
  import FFTW.{INSTANCE => fftw}
  def fftw3j_1D(src: Array[Double], n: Int): Array[Double] = {

    val flags: Int = FFTW.FFTW_ESTIMATE
    val dst = new Array[Double](_length = src.size)

    val sizeofDouble = NativeLong.SIZE;
    val inBytes  = sizeofDouble*n*2
    val outBytes = sizeofDouble*n*2

    val in = fftw.fftw_malloc(new NativeLong(inBytes))
    val out = fftw.fftw_malloc(new NativeLong(outBytes))
    val inbuf = in.getByteBuffer(0, inBytes).asDoubleBuffer()
    val outbuf = out.getByteBuffer(0, outBytes).asDoubleBuffer()

    val planForward  = fftw.fftw_plan_dft_1d(n, inbuf, outbuf, FFTW.FFTW_FORWARD, flags)

    inbuf.clear()
    inbuf.put(src)
    fftw.fftw_execute(planForward)
    outbuf.rewind()
    outbuf.get(dst)

    fftw.fftw_destroy_plan(planForward)
    fftw.fftw_free(in)
    fftw.fftw_free(out)

    dst
  }
}
