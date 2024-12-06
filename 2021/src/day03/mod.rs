use nalgebra::*;

type BitMatrix = DMatrix<u8>;
type DiagnosticReport = BitMatrix;

fn parse(input: String) -> DiagnosticReport {
    let lines = input.lines();
    let width = lines.clone().map(|line| line.len()).min().unwrap();
    let height = lines.clone().count();
    let rows = lines.flat_map(|line| {
        line.chars().filter_map(|c| match c {
            '0' => Some(0),
            '1' => Some(1),
            _ => None,
        })
    });
    BitMatrix::from_iterator(width, height, rows).transpose()
}

fn to_dec(bits: &[u8]) -> u32 {
    bits.iter()
        .rev()
        .enumerate()
        .map(|(n, bit)| *bit as u32 * (2 as u32).pow(n as u32))
        .sum()
}

fn most_common(col: DVectorView<u8>) -> u8 {
    let ones = col.iter().filter(|bit| **bit == 1).count();
    let zeroes = col.iter().filter(|bit| **bit == 0).count();
    if ones >= zeroes {
        1
    } else {
        0
    }
}

fn least_common(col: DVectorView<u8>) -> u8 {
    let ones = col.iter().filter(|bit| **bit == 1).count();
    let zeroes = col.iter().filter(|bit| **bit == 0).count();
    if ones >= zeroes {
        0
    } else {
        1
    }
}

pub fn part1(input: String) -> u32 {
    let diagnostic_report = parse(input);
    let (gamma_rate, epsilon_rate): (Vec<u8>, Vec<u8>) = diagnostic_report
        .column_iter()
        .map(|col| (most_common(col), least_common(col)))
        .unzip();
    to_dec(&gamma_rate) * to_dec(&epsilon_rate)
}

fn filter<F>(bit_criteria: F, diagnostic_report: DiagnosticReport) -> Vec<u8>
where
    F: Fn(DVectorView<u8>) -> u8,
{
    //std::env::set_var("RUST_BACKTRACE", "1");
    diagnostic_report
        .clone()
        .column_iter()
        .enumerate()
        .map(|(i, _)| i)
        .fold(diagnostic_report, |report, i| {
            if report.row_iter().count() == 1 {
                dbg!(report.clone());
                return report;
            }
            let bit = bit_criteria(report.column(i));
            let invalid_rows = report
                .row_iter()
                .enumerate()
                .filter(|(_, row)| row[i] != bit)
                .map(|(i, _)| i)
                .collect::<Vec<usize>>();
            let new_report = report.remove_rows_at(&invalid_rows);
            new_report
        })
        .row(0)
        .iter()
        .map(|b| *b)
        .collect()
}

pub fn part2(input: String) -> u32 {
    let diagnostic_report = parse(input);
    let oxygen_generator_rating = to_dec(&filter(most_common, diagnostic_report.clone()));
    let c02_scrubber_rating = to_dec(&filter(least_common, diagnostic_report));
    dbg!(oxygen_generator_rating);
    dbg!(c02_scrubber_rating);
    oxygen_generator_rating * c02_scrubber_rating
}
