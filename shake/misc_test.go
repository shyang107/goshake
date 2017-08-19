package shake

import (
	"testing"

	"github.com/cpmech/gosl/io"
)

func Test_absxmax(t *testing.T) {
	type args struct {
		x []float64
	}
	tests := []struct {
		name      string
		args      args
		wantNxmax int
		wantXmax  float64
	}{
		{
			name:      "xmax_case 1",
			args:      args{[]float64{-1., -3.2, 4.7, -5.2, 3.5, 2.5}},
			wantNxmax: 3,
			wantXmax:  5.2,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotNxmax, gotXmax := absxmax(tt.args.x)
			io.Pf("x[] =[ ")
			for i, v := range tt.args.x {
				io.Pf("%d:%v ", i, v)
			}
			io.Pf("]\n")
			io.Pf("nxmax, xmax = %d, %v\n", gotNxmax, gotXmax)
			if gotNxmax != tt.wantNxmax {
				t.Errorf("xmax() gotNxmax = %v, want %v", gotNxmax, tt.wantNxmax)
			}
			if gotXmax != tt.wantXmax {
				t.Errorf("xmax() gotXmax = %v, want %v", gotXmax, tt.wantXmax)
			}
		})
	}
}
