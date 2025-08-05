import numpy as np
import pandas as pd
import os
import glob
import matplotlib.pyplot as plt

# Folder containing your CSV files
folder_path = "./playerMovement"  # Input folder
output_folder = "./analysisResults"  # Output folder

os.makedirs(output_folder, exist_ok=True)  # Create output folder if it doesn't exist

csv_files = glob.glob(os.path.join(folder_path, "*.csv"))

results = []

for file_path in csv_files:
    df = pd.read_csv(file_path, sep=';')
    numeric_cols = ['Pos X', 'Pos Y', 'Pos Z', 'Bewegung X', 'Bewegung Y', 'Bewegung Z']
    for col in numeric_cols:
        if df[col].dtype == object:
            df[col] = df[col].str.replace(',', '.').astype(float)
        else:
            df[col] = df[col].astype(float)

    # Assume the first column is time (e.g., 'Time' or similar)
    time_col = df.columns[0]
    df[time_col] = df[time_col].str.replace(',', '.').astype(float)
    time_diffs = df[time_col].diff().fillna(0)

    positions = df[['Pos X', 'Pos Y', 'Pos Z']].to_numpy()
    diffs = np.linalg.norm(np.diff(positions, axis=0), axis=1)
    movement_magnitude = np.insert(diffs, 0, 0.0)
    df['movement_magnitude'] = movement_magnitude
    df['is_moving'] = df['movement_magnitude'] > 0.001

    walking_time = time_diffs[df['is_moving']].sum()
    stopping_time = time_diffs[~df['is_moving']].sum()
    overall_time = df[time_col].iloc[-1] - df[time_col].iloc[0]

    # Calculate walking/looking/standing times BEFORE filtering for stops
    artwork_col = [col for col in df.columns if "art" in col.lower()][0]
    is_looking = df[artwork_col].str.strip().str.lower() != "none"
    not_looking = ~is_looking

    walking_and_looking_time = time_diffs[df['is_moving'] & is_looking].sum()
    walking_and_not_looking_time = time_diffs[df['is_moving'] & not_looking].sum()
    standing_and_looking_time = time_diffs[~df['is_moving'] & is_looking].sum()
    standing_and_not_looking_time = time_diffs[~df['is_moving'] & not_looking].sum()

    walking_and_looking_time = round(float(walking_and_looking_time), 2)
    walking_and_not_looking_time = round(float(walking_and_not_looking_time), 2)
    standing_and_looking_time = round(float(standing_and_looking_time), 2)
    standing_and_not_looking_time = round(float(standing_and_not_looking_time), 2)

    # Now filter for valid stops as before
    df['stop_id'] = (df['is_moving'] != df['is_moving'].shift()).cumsum()
    stop_groups = df[~df['is_moving']].groupby('stop_id')
    valid_stops = []
    for stop_id, group in stop_groups:
        stop_duration = time_diffs.loc[group.index].sum()
        if stop_duration > 1:
            valid_stops.append(stop_id)
        else:
            # Mark these short stops as moving (count them as walking)
            df.loc[group.index, 'is_moving'] = True

    # After updating is_moving, recalculate stop_ids and stop_groups
    df['stop_id'] = (df['is_moving'] != df['is_moving'].shift()).cumsum()
    stop_groups = df[~df['is_moving']].groupby('stop_id')
    num_stops = stop_groups.ngroups

    # Calculate stop durations in seconds (only valid stops remain)
    stop_durations = []
    for stop_id, group in stop_groups:
        stop_duration = round(float(time_diffs.loc[group.index].sum()), 2)
        stop_durations.append(stop_duration)

    # Calculate time spent looking at each artwork (total)
    artwork_times = {}
    for artwork in df[artwork_col].unique():
        if artwork and artwork.strip().lower() != "none":
            mask = df[artwork_col] == artwork
            time_spent = time_diffs[mask].sum()
            artwork_times[artwork.strip()] = round(float(time_spent), 2)

    # Calculate overall time spent looking at any artwork (not "none")
    overall_looking_time = time_diffs[is_looking].sum()
    overall_looking_time = round(float(overall_looking_time), 2)

    room_number = os.path.basename(file_path).split('_')[1].replace('.csv', '')
    room_number = ''.join(filter(lambda c: not c.isdigit(), room_number))
    result_row = {
        "Room": room_number,
        "overall_time (in sec)": overall_time,
        "walking_time (in sec)": walking_time,
        "stopping_time (in sec)": stopping_time,
        "num_stops": num_stops,
        "stop_durations (in sec)": stop_durations,
        "walking_and_looking (sec)": walking_and_looking_time,
        "walking_and_not_looking (sec)": walking_and_not_looking_time,
        "standing_and_looking (sec)": standing_and_looking_time,
        "standing_and_not_looking (sec)": standing_and_not_looking_time,
        "overall_looking_time (sec)": overall_looking_time
    }
    # Add artwork times as columns
    for artwork, t in artwork_times.items():
        result_row[f"time_{artwork} (sec)"] = t
    results.append(result_row)

    # Save plot as image in output folder
    plt.figure()
    plt.plot(positions[:, 0], positions[:, 2], label='Walking Path')
    plt.xlabel('Pos X')
    plt.ylabel('Pos Z')
    plt.legend()
    plt.title(f'2D Walking Path: {os.path.basename(file_path)}')
    plt.axis('equal')
    img_name = room_number + "_walking_path.png"
    plt.savefig(os.path.join(output_folder, img_name))
    plt.close()

# Convert results to DataFrame
results_df = pd.DataFrame(results)

# Save to Excel in output folder
results_df.to_excel(os.path.join(output_folder, "walking_analysis_results.xlsx"), index=False)
